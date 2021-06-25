*          DATA SET SPREPI302  AT LEVEL 053 AS OF 05/10/02                      
*PHASE SPI302A                                                                  
*INCLUDE MEDBDESC                                                               
*INCLUDE SPRPFOOT                                                               
*INCLUDE REPCALOV                                                               
         PRINT NOGEN                                                            
         TITLE 'SPREPI302 - INVOICE CHECKING LIST'                              
SPI302   CSECT                                                                  
         NMOD1 0,SPI302,RR=R5                                                   
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         L     R2,=V(SP60WK)                                                    
         AR    R2,R5                                                            
         USING SP60WK,R2                                                        
         ST    R5,RELO                                                          
         L     R7,MEDBUFF                                                       
         USING MEDBLOCK,R7                                                      
         USING MEDDATA,R4                                                       
         USING SUMDSECT,R3                                                      
         STM   RA,RC,SP60RA                                                     
         ST    R2,SP60R2                                                        
BYPW     DS    0H                                                               
         CLI   MODE,ESTFRST                                                     
         BNH   *+12                                                             
         CLI   QSTART,C' '                                                      
         BE    EXIT                                                             
         GOTO1 =V(SPRPFOOT),DMCB,(RA),RR=RELO                                   
         CLI   MODE,RUNFRST                                                     
         BNE   M1                                                               
         MVC   SVMAXLIN,MAXLINES                                                
         MVI   RQDAYPT,C'Y'                                                     
         MVI   RQEQUIV,C'Y'                                                     
         MVI   RQMGOPT,C'Y'                                                     
         LA    RF,ICSDESC                                                       
         ST    RF,APTSDESC                                                      
         MVI   MEDEXTAC,C'Y'                                                    
         ST    RF,APRSDESC                                                      
         L     RF,=V(PNTABLE)                                                   
         A     RF,RELO                                                          
         ST    RF,VPNTABLE                                                      
         L     RF,=V(SSTABLE)                                                   
         A     RF,RELO                                                          
         ST    RF,VSSTABLE                                                      
         L     RF,=V(SORTC)                                                     
         A     RF,RELO                                                          
         ST    RF,VRSORT                                                        
         L     RF,=V(SPRPFOOT)                                                  
         A     RF,RELO                                                          
         ST    RF,VFOOT                                                         
         L     RF,=V(SVMDBLK)                                                   
         A     RF,RELO                                                          
         ST    RF,VSVMDBLK                                                      
         L     RF,=V(HDATES)                                                    
         A     RF,RELO                                                          
         ST    RF,VHDATES                                                       
         L     RF,=V(GETBUF)                                                    
         A     RF,RELO                                                          
         ST    RF,VGETBUF                                                       
         L     RF,=V(STATOTC)                                                   
         A     RF,RELO                                                          
         ST    RF,VSTATOT                                                       
         L     RF,=V(SUBPAREA)                                                  
         A     RF,RELO                                                          
         ST    RF,VSUBPARA                                                      
         LA    RE,MYHEAD                                                        
         ST    RE,HEADHOOK                                                      
         L     RE,=V(GETREP)                                                    
         A     RE,RELO                                                          
         ST    RE,VGETREP                                                       
         L     RE,=V(COMPRNT)                                                   
         A     RE,RELO                                                          
         ST    RE,VCOMPRNT                                                      
         L     RE,=V(REPCALOV)                                                  
         A     RE,RELO                                                          
         ST    RE,REPCALOV                                                      
         LA    RE,SPTHOOK                                                       
         ST    RE,SPOTHOOK                                                      
         L     RE,VMASTC                                                        
         USING MASTD,RE                                                         
         OC    MCREMPQK,MCREMPQK   TEST FOR SOON JOB                            
         BNZ   *+8                                                              
         MVI   RCREQREP,C'N'       SUPPRESS REQUEST PAGE IF NOT                 
         MVI   FIRST,1                                                          
         B     EXIT                                                             
         SPACE 2                                                                
M1       CLI   MODE,MKTFRST                                                     
         BL    M2                                                               
         CLI   ESTACT,0            ANY ESTIMATES FOR PRODUCT                    
         BE    EXIT                                                             
         EJECT                                                                  
M2       CLI   MODE,REQFRST                                                     
         BNE   M3                                                               
         MVI   HAVPROD,C'N'                                                     
         GOTO1 =V(RQFIRST),RR=RELO                                              
         L     RF,=V(PGRIDC)                                                    
         A     RF,RELO                                                          
         ST    RF,VPGRID                                                        
         SPACE 2                                                                
         GOTO1 =V(RQFRSTA),RR=RELO                                              
         MVI   SUBPSW,0                                                         
         MVI   MODE,RUNFRST                                                     
         BAS   R9,GOTOSUB                                                       
         MVI   MODE,REQFRST                                                     
M2A      BAS   R9,GOTOSUB                                                       
         MVI   FIRST,0                                                          
         B     EXIT                                                             
         EJECT                                                                  
M3       CLI   MODE,CLTFRST                                                     
         BNE   M4                                                               
         GOTO1 MEDPRDRD,DMCB,(RA)                                               
M4       CLI   MODE,ESTFRST                                                     
         BNE   M5                                                               
         MVI   ESTACT,1                                                         
         MVI   PASS,0                                                           
         CLI   HAVPROD,C'Y'                                                     
         BE    M4B                                                              
         MVC   HLDPNAM(3),QPRD                                                  
         MVC   QPRD,=C'ALL'                                                     
         BAS   R9,GOTOSUB                                                       
         MVC   QPRD,HLDPNAM                                                     
         MVI   HAVPROD,C'Y'                                                     
         SPACE 2                                                                
M4B      GOTO1 =V(EFRSTC),RR=RELO                                               
         CLI   SPOTPROF+1,0                                                     
         BNE   *+8                                                              
         MVI   SPOTPROF+1,C'N'                                                  
         CLI   SPOTPROF+5,20                                                    
         BL    *+8                                                              
         MVI   SPOTPROF+5,0                                                     
         MVI   REQPRD2,0                                                        
         CLC   QPRD2,=C'   '       CHECK FOR PIGGY REQUEST                      
         BE    EXIT                                                             
         CLC   QPRD2,=C'ALL'                                                    
         BE    EXIT                                                             
         CLC   QPRD2,=C'POL'                                                    
         BE    EXIT                                                             
         LA    RE,220              SAVE PRODUCT 2 NUMBER                        
         L     RF,PRDBUFF                                                       
         CLC   QPRD2,1(RF)                                                      
         BE    *+14                                                             
         AH    RF,PRDBUFLN                                                      
         BCT   RE,*-14                                                          
         DC    H'0'                                                             
         MVC   REQPRD2,0(RF)                                                    
         B     EXIT                                                             
         SPACE 2                                                                
M5       CLI   MODE,PRDFRST                                                     
         BNE   M6                                                               
         MVI   PASS,0                                                           
         MVI   ESTACT,0            RESET ESTIMATE ACTIVE SWITCH                 
         XC    CURRREP,CURRREP                                                  
         XC    REPLIST,REPLIST                                                  
         B     EXIT                                                             
         EJECT                                                                  
M6       CLI   MODE,PROCBUY                                                     
         BNE   M8                                                               
         MVI   SORTPASS,1          SET SORT FOR STORE                           
         CLI   QREPTYPE,C'S'       SPECIAL REP ONLY                             
         BNE   M6NSREP                                                          
         MVC   CURRREP,BREP                                                     
         L     R5,ADBUY                                                         
         USING BUYREC,R5                                                        
         CLC   BDREP,BREP          REQUESTED REP                                
         BNE   EXIT                                                             
         B     M6NSREP1                                                         
M6NSREP  CLC   IQREP,=C'   '        ACCEPT ALL BUYS                             
         BE    M6SREP                                                           
         L     R5,ADBUY            NO - BYPASS SPECIAL REP BUYS                 
         OC    BDREP,BDREP                                                      
         BNZ   EXIT                                                             
         B     M6NSREP1                                                         
         SPACE 2                                                                
M6SREP   OC    CURRREP,CURRREP     PROCESSING A PARTICULAR REP                  
         BZ    M6SRSAV              NO - BUILD LIST OF REPS                     
         L     R5,ADBUY             YES - FILTER ON REP                         
         CLC   BDREP,CURRREP                                                    
         BNE   EXIT                                                             
         B     M6NSREP1                                                         
         SPACE 2                                                                
M6SRSAV  L     R5,ADBUY            SAVE REPS FOR SUSEQUENT PROCESSING           
         OC    BDREP,BDREP                                                      
         BZ    M6NSREP1                                                         
         LA    RE,REPLIST                                                       
M6SRSAV1 OC    0(2,RE),0(RE)                                                    
         BNZ   *+14                                                             
         MVC   0(2,RE),BDREP                                                    
         B     EXIT                                                             
         CLC   0(2,RE),BDREP                                                    
         BE    EXIT                                                             
         LA    RE,2(RE)                                                         
         B     M6SRSAV1                                                         
M6NSREP1 DS    0C                                                               
         SPACE 2                                                                
         CLC   QPRD2,=C'   '       PIGGYBACK REQUEST                            
         BE    M6PIGXIT             NO - ACCEPT BUY                             
         CLI   BUYKEY+3,X'FF'      POL BUY                                      
         BE    M6PIGXIT             YES - ACCEPT BUY                            
         CLC   BPRD,BUYKEY+3       EQUAL IF ACTIVE PARTNER                      
         BNE   EXIT                 BYPASS PASSIVE BUYS ON PIGGY REQ.           
         LA    R6,BDELEM           LOOK FOR PIGGYBACK ELEM                      
         USING PBELEM,R6                                                        
M6PIGNXT ZIC   RE,1(R6)                                                         
         AR    R6,RE                                                            
         CLI   0(R6),0             EXIT IF NO PIGGYPACK ELEMENT                 
         BE    EXIT                                                             
         CLI   0(R6),4                                                          
         BNE   M6PIGNXT                                                         
         CLC   QPRD2,=C'ALL'       BUY OK IF ALL PRODUCTS WANTED                
         BE    M6PIGXIT                                                         
         CLC   PBPROD,REQPRD2      PIGGY PRODUCT=REQUESTED                      
         BNE   EXIT                 NO - EXIT                                   
         DROP  R5                                                               
         DROP  R6                                                               
M6PIGXIT DS    0C                                                               
         SPACE 2                                                                
         BAS   R9,GOTOSUB                                                       
M6A1SRT1 L     RE,VPGRID                                                        
         L     RF,=F'15000'                                                     
         XCEF                                                                   
         XC    PGNOENT,PGNOENT                                                  
         XC    HIATAB,HIATAB                                                    
         XC    PREMTAB,PREMTAB                                                  
M6A1SRT2 CLI   SORTREQ,1                                                        
         BNE   M6A1SRT3                                                         
         GOTO1 VRSORT                                                           
         CLI   SORTPASS,1          BUILD PASS                                   
         BE    SORTX                YES - EXIT                                  
         CLI   SORTPASS,3          END OF SORT                                  
         BE    SORTX                YES - EXIT                                  
M6A1SRT3 DS    0H                                                               
         MVI   LINEACT,0           RESET LINE ACTIVE SWITCH                     
         GOTO1 =V(EXTRCT),RR=RELO                                               
         CLI   MEDSPILL,C'Y'                                                    
         BNE   *+12                                                             
         CLI   SPOTPROF+5,2                                                     
         BNE   SORTX                                                            
         LA    R5,MEDPERD                                                       
         L     R4,4(R5)                                                         
         OC    MEDBYD(12),MEDBYD                                                
         BNZ   M6A1SRT4                                                         
         CLI   LINEACT,0           ANY ACTIVITY FOR THIS LINE                   
         BE    SORTX                NO - TRY NEXT LINE                          
M6A1SRT4 DS    0C                                                               
         MVI   BUYACT,1                                                         
M6A      DS    0H                                                               
         GOTO1 =V(VMDBDESC),DMCB,(RA),PRTLINE,RR=RELO                           
         GOTO1 =V(GETCAP),DMCB,(RA),RR=RELO                                     
         MVI   CURRSORT,X'FF'                                                   
         LA    R5,PRTLINE                                                       
         L     RF,DDESC            MOVE IN DESCRIPTION                          
         BASR  R9,RF                                                            
         MVC   P2+86(L'PKGAREA),PKGAREA                                         
         L     R7,MEDBUFF                                                       
         LA    R5,MEDPERD                                                       
         L     R4,4(R5)                                                         
         USING MEDDATA,R4                                                       
         EDIT  MEDBYD,(14,P+96),2,COMMAS=YES,MINUS=YES,ALIGN=LEFT               
         EDIT  MEDBYNET,(14,P+110),2,COMMAS=YES,MINUS=YES,ALIGN=LEFT            
         MVI   P2,0                                                             
         LA    RE,P2                                                            
         ST    RE,FULL                                                          
         GOTO1 VCOMPRNT,DMCB,(RA)                                               
         MVC   SVDESCL,P1                                                       
         ZIC   RE,MAXLINES                                                      
         ZIC   RF,LINE                                                          
         SR    RE,RF                                                            
         CH    RE,=H'8'                                                         
         BH    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 REPORT                                                           
         MVC   SVDESCL+86(45),SPACES                                            
         MVC   SVDESCL+86(9),=C'CONTINUED'                                      
         L     R6,DSTAGRID         SET GRID ADDRESS                             
         BAS   R9,BGRID                                                         
         SPACE 2                                                                
         SPACE 2                                                                
         B     SORTX                                                            
         SPACE 2                                                                
SORTX    CLI   SORTREQ,1                                                        
         BNE   EXIT                                                             
         CLI   SORTPASS,2          GET PASS                                     
         BNE   EXIT                 NO - EXIT                                   
         CLC   CURRSORT,NEXTSORT                                                
         BE    M6A1SRT2                                                         
         B     M6A1SRT1                                                         
         EJECT                                                                  
M8       CLI   MODE,STAFRST                                                     
         BNE   M10                                                              
         BAS   R9,GOTOSUB                                                       
         GOTO1 =V(GETSADDR),RR=RELO                                             
         XC    PDNCNTR,PDNCNTR                                                  
         XC    SSCNTR,SSCNTR                                                    
         XC    STACINV,STACINV     SET INVOICE COST                             
         MVI   BUYACT,0                                                         
         MVI   SPOTACT,0           RESET SPOT ACTIVITY                          
         CLI   IQINV+9,X'C0'                                                    
         BL    EXIT                                                             
         TM    IQINV,X'F0'                                                      
         BNO   EXIT                                                             
         PACK  DUB,IQINV                                                        
         CVB   RE,DUB                                                           
         ST    RE,STACINV                                                       
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
M10      CLI   MODE,STALAST                                                     
         BNE   M12                                                              
         CLI   SORTREQ,1           SORT REQUIRED                                
         BNE   M10A1                                                            
         MVI   SORTPASS,2           YES - SET SORT PASS FOR EXTRACT             
         MVI   MODE,PROCBUY                                                     
         GOTO1 SORT2                                                            
         MVI   MODE,STALAST                                                     
M10A1    DS    0H                                                               
M10A12   DS    0C                                                               
         XC    SVDESCL,SVDESCL                                                  
         CLI   SPOTACT,0           ANY SPOT ACTIVITY                            
         BE    M10A14                                                           
         CLI   BUYACT,0                                                         
         BNE   M10A13                                                           
         CLI   PROGPROF,C'Y'       SUPPRESS MESSAGE                             
         BE    M10A14                                                           
         XC    P1(132),P1                                                       
         MVC   P1+20(20),=C'ALL BUYS ARE CLEARED'                               
         GOTO1 REPORT                                                           
         B     M10A14                                                           
M10A13   DS    0C                                                               
         GOTO1 VSTATOT                                                          
M10A14   BAS   R9,GOTOSUB                                                       
         MVI   FORCEHED,C'Y'                                                    
         CLI   PASS,0              END OF DATES FOR THIS PASS                   
         BNE   M14A15A             NO - TRY NEXT DATE                           
         LA    RE,REPLIST          PROCESS SPECIAL REPS                         
M14A15   OC    0(2,RE),0(RE)                                                    
         BZ    M14A16               CLEAR OUT REP LIST                          
         CLI   0(RE),X'FF'                                                      
         BNE   *+12                                                             
         LA    RE,2(RE)                                                         
         B     M14A15                                                           
         MVC   CURRREP,0(RE)                                                    
         MVI   0(RE),X'FF'                                                      
M14A15A  MVI   MODE,REREAD                                                      
         B     EXIT                                                             
*                                                                               
M14A16   XC    CURRREP,CURRREP                                                  
         XC    REPLIST,REPLIST                                                  
         B     EXIT                                                             
         SPACE 2                                                                
*        TRAP FINAL SORT EXIT                                                   
SORT2    NTR1                                                                   
         B     M6A1SRT1                                                         
         SPACE 2                                                                
         SPACE 2                                                                
M12      CLI   MODE,MKTFRST                                                     
         BNE   M14                                                              
         MVI   BUYACT,0                                                         
M12A     DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         BAS   R9,GOTOSUB                                                       
         B     EXIT                                                             
         SPACE 2                                                                
M14      CLI   MODE,MKTLAST                                                     
         BNE   M16                                                              
M14A1    BAS   R9,GOTOSUB                                                       
M14B     MVI   PASS,0                                                           
         MVI   FORCEHED,C'Y'                                                    
         B     EXIT                                                             
         EJECT                                                                  
M16      DS    0C                                                               
         EJECT                                                                  
         SPACE 2                                                                
M18      DS    0C                                                               
M34      CLI   MODE,REQLAST                                                     
         BNE   M36                                                              
         CLI   FOOT1,C' '                                                       
         BE    M36                                                              
         MVI   FORCEHED,C'N'                                                    
         MVI   P,0                                                              
         MVC   P2(132),FOOT1                                                    
         GOTO1 REPORT                                                           
         SPACE 2                                                                
M36      BAS   R9,GOTOSUB                                                       
EXIT     XMOD1 1                                                                
         EJECT                                                                  
GOTOSUB  CLI   PASS,0              PASS = 0                                     
         BNER  R9                   NO - BYPASS SUBPROGRAM                      
         CLI   SUBPSW,0                                                         
         BNE   *+10                                                             
         CLI   MODE,ESTFRST                                                     
         BHR   R9                                                               
         MVC   SVOPTS,QOPT1                                                     
         MVC   QOPT1(7),MSOPT                                                   
         MVC   SVPROF,PROGPROF                                                  
         MVC   PROGPROF,MSPROF                                                  
         MVC   SVSPECS,SPECS       GO TO SUBPROGRAM                             
         MVC   SVSUPMKT,SPSUPMKT                                                
         MVC   SVMDTAB,MEDTABLE                                                 
         MVC   SPECS,SVPH01        SET SPECS                                    
         MVC   MEDTABLE,SVPH04                                                  
         OC    MSBFHOOK,MSBFHOOK                                                
         BZ    GOSUB1                                                           
         L     RF,BUFFBUFF                                                      
         USING BUFFALOD,RF                                                      
         MVC   BUFFHOOK,MSBFHOOK                                                
         DROP  RF                                                               
GOSUB1   DS    0C                                                               
         CLI   MODE,MKTLAST                                                     
         BL    GOTOSUB1                                                         
         MVI   FORCEMID,C'N'                                                    
         MVC   MID1,SPACES                                                      
         MVC   MID2,SPACES                                                      
         MVI   FORCEHED,C'Y'                                                    
GOTOSUB1 MVC   SVRCSUB,RCSUBPRG                                                 
         L     RF,MEDBUFF          RESTORE MEDIA SUMMARY DATES                  
         L     RE,VSVMDBLK                                                      
         LA    R1,1208                                                          
         MOVE  ((RF),(R1)),(RE)                                                 
         MVC   SVSPHK,SPOTHOOK                                                  
         MVC   SPOTHOOK,MSSPHK                                                  
         MVC   SVHDHOOK,HEADHOOK                                                
         MVC   HEADHOOK,MSHDHOOK                                                
         MVC   RCSUBPRG,MSRCSUB                                                 
         MVC   SPSUPMKT,MSSUPMKT                                                
         L     RF,SVPH02                                                        
         GOTO1 (RF),DMCB,(RA)                                                   
         L     RF,BUFFBUFF                                                      
         USING BUFFALOD,RF                                                      
         MVC   MSBFHOOK,BUFFHOOK                                                
         XC    BUFFHOOK,BUFFHOOK                                                
         DROP  RF                                                               
         MVC   MSOPT,QOPT1                                                      
         MVC   QOPT1(7),SVOPTS                                                  
         MVC   MSSUPMKT,SPSUPMKT                                                
         MVC   SPSUPMKT,SVSUPMKT                                                
         MVC   PROGPROF,SVPROF                                                  
         L     RE,MEDBUFF                                                       
         L     RF,VSVMDBLK                                                      
         LA    R1,1208                                                          
         MOVE  ((RF),(R1)),(RE)                                                 
         MVC   MSSPHK,SPOTHOOK                                                  
         MVC   SPOTHOOK,SVSPHK                                                  
         MVC   MSHDHOOK,HEADHOOK                                                
         MVC   MSRCSUB,RCSUBPRG                                                 
         MVC   RCSUBPRG,SVRCSUB                                                 
         MVC   HEADHOOK,SVHDHOOK                                                
         MVC   SPECS,SVSPECS                                                    
         MVC   MEDTABLE,SVMDTAB                                                 
         BR    R9                                                               
         EJECT                                                                  
* BUILD WEEKLY GRID AND SUM INTO STATION BUCKETS  R6=PRINT POSITION             
BGRID    L     R5,MEDAFRST                                                      
         LA    R8,STAGRID                                                       
         ST    R6,FULL             SAVE PRINT LINE ADDRESS                      
BGRID2   L     R4,4(R5)                                                         
         OC    0(4,R5),0(R5)                                                    
         BZ    BGRID5                                                           
BGRID4   L     RE,0(R8)            SUM WEEKLY SPOTS                             
         A     RE,MEDBYSPT                                                      
         ST    RE,0(R8)                                                         
         LA    R6,4(R6)                                                         
         LA    R8,4(R8)                                                         
         LA    R5,12(R5)                                                        
         B     BGRID2                                                           
BGRID5   CLI   MODE,PROCBUY        PRINT PREMPTIONS                             
         BNE   BGRIDX                                                           
         L     R4,MEDPERD+4                                                     
         L     RE,STACOST                                                       
         A     RE,MEDBYD                                                        
         ST    RE,STACOST                                                       
         L     RE,STASPOT                                                       
         A     RE,MEDBYSPT                                                      
         ST    RE,STASPOT                                                       
         L     RE,STACOSTN                                                      
         A     RE,MEDBYNET                                                      
         ST    RE,STACOSTN                                                      
         CLC   QPROG,=C'I3'        REQUEST REPORT                               
         BNE   BGRID6               NO-REPORT INVOICE DATA                      
         L     RE,STACINV           YES-REPORT CLEARED DATA                     
         A     RE,MEDBYNPY                                                      
         ST    RE,STACINV                                                       
BGRID6   DS    0H'0'                                                            
         GOTO1 =V(PTSGRID),DMCB,(LENGRID,(RA)),RR=RELO                          
BGRIDX   BR    R9                                                               
         EJECT                                                                  
         USING BDEXTD,R5                                                        
ICSDESC  MVC   P1(L'BDPEST),BDPEST      SET UP BUY DESCRIPTION LINE             
         MVI   P1+3,C'-'                                                        
         MVC   P1+4(L'BDPLIN),BDPLIN                                            
         MVC   P1+8(L'BDPBDATE),BDPBDATE                                        
         CLI   P1+14,0                                                          
         BE    *+8                                                              
         CLI   P1+14,C' '                                                       
         BE    *+8                                                              
         MVI   P1+13,C'-'                                                       
         MVC   P1+20(L'BDPWKS),BDPWKS                                           
         MVC   P1+24(L'BDPDAY),BDPDAY                                           
         MVC   P1+34(L'BDPNPWK),BDPNPWK                                         
         MVC   P1+37(L'BDPTIME),BDPTIME                                         
         MVC   P1+49(L'BDPDPT),BDPDPT                                           
         MVC   P1+51(L'BDPSLN),BDPSLN                                           
         MVC   P1+55(L'BDPPROG),BDPPROG                                         
         MVC   P1+73(L'BDPCOST),BDPCOST                                         
         BR    R9                                                               
         EJECT                                                                  
         EJECT                                                                  
* DO SUMMARIES FOR VARIOUS BREAKS                                               
DOSUM    NTR1                                                                   
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 MEDADDWT,DMCB,(RA)                                               
         CLI   SPDUPTOT,C'Y'                                                    
         BE    DOSUM2                                                           
         MVC   WEIGHT,SPWEIGHT                                                  
DOSUM2   SR    R9,R9                                                            
         IC    R9,LEVEL                                                         
         L     R8,BUFFBUFF                                                      
         GOTO1 BUFFALO,DMCB,=C'CLEAR',(BUFCDE,(R8)),(X'80',(R9))                
         MVI   FORCEHED,C'Y'                                                    
         BAS   R9,GOTOSUB                                                       
         MVI   FORCEHED,C'Y'                                                    
         XIT1                                                                   
         EJECT                                                                  
* SPOT HOOK ROUTINES                                                            
         DS    0D                                                               
         USING *,RF                                                             
SPTHOOK  NTR1  BASE=SP60RB                                                      
         DROP  RF                                                               
         L     R2,SP60R2                                                        
         LM    RA,RC,SP60RA                                                     
         L     R6,SPOTADDR                                                      
         USING REGELEM,R6                                                       
         CLI   0(R6),X'0A'         BRAND MODE - CANNOT BE HIATUS                
         BL    *+12                                                             
         CLI   1(R6),14            BYPASS IF POL UNA OR HIA                     
         BL    BYPHIA                                                           
         CLI   QPRD2,C' '          FILTER OUT POL PIGGYBACK REQUESTS            
         BE    SHK0                                                             
         CLC   QPRD2(3),=C'POL'                                                 
         BE    SHK0                                                             
         CLC   QPRD2(3),=C'ALL'                                                 
         BE    SHK0                                                             
         L     R6,SPOTADDR                                                      
         USING REGELEM,R6                                                       
         CLI   0(R6),X'0A'         BYPASS BRAND FILTERING                       
         BL    SHK0                                                             
         CLI   1(R6),15            BYPASS SPOT IF NOT PIGGYBACK                 
         BL    BYPHIA                                                           
         ZIC   RF,RPPRD+4          CHECK PIGGY PARTNER                          
         BCTR  RF,0                                                             
         MH    RF,PRDBUFLN                                                      
         A     RF,PRDBUFF                                                       
         CLC   QPRD2(3),1(RF)      ACCEPT IF REQ. PRODUCT                       
         BE    SHK0                                                             
BYPHIA   MVI   SPOTYORN,C'N'       BYPASS IF NOT REQ. PRD OR HIA/UNA            
         B     SHKXIT                                                           
         SPACE 2                                                                
SHK0     CLI   QOPT1,C' '          UNCLEARED ONLY                               
         BE    *+8                                                              
         CLI   QOPT1,C'U'                                                       
         BE    *+8                                                              
         B     SHK1                                                             
         OC    RPAY,RPAY                                                        
         BNZ   BYPSPOT                                                          
SHK1     CLI   QOPT1,C'P'          UNCLEARED ITEMS ONLY                         
         BNE   SHK2                                                             
         OC    RPAY,RPAY                                                        
         BZ    BYPSPOT                                                          
         SPACE 2                                                                
SHK2     CLI   QOPT2,C'U'          UNAFFIDED SPOTS ONLY                         
         BNE   SHK3                                                             
         LR    R9,R6                                                            
         ZIC   RE,1(R6)                                                         
         AR    R9,RE                                                            
         CLI   0(R9),X'10'                                                      
         BE    BYPSPOT                                                          
SHK3     B     SPTHOOKX                                                         
         SPACE 2                                                                
BYPSPOT  MVI   SPOTYORN,C'N'                                                    
SPTHOOKX DS    0C                                                               
         MVI   SPOTACT,1           SET FOR ACTIVE SPOTS                         
         CLI   SPOTYORN,C'Y'       IS SPOT ACTIVE                               
         BNE   *+8                  NO - EXIT                                   
         MVI   LINEACT,1            YES - SET LINE IS ACTIVE                    
         DROP  R6                                                               
SHKXIT   XIT1                                                                   
         EJECT                                                                  
* HEADLINE ROUTINES                                                             
         DS    0D                                                               
         USING *,RF                                                             
MYHEAD   NTR1  BASE=SP60RB                                                      
         DROP  RF                                                               
         L     R2,SP60R2                                                        
         LM    RA,RC,SP60RA                                                     
         OC    CURRREP,CURRREP                                                  
         BZ    MYHDNOW                                                          
         MVC   H7+49(26),=C'***SPECIAL REP *** ONLY***'                         
         GOTO1 VRCPACK,DMCB,(C'U',CURRREP),H7+64                                
*                                                                               
MYHDNOW  DS    0H                                                               
         MVI   H9,C'-'                                                          
         MVC   H9+1(131),H9                                                     
         MVC   H11,H9                                                           
         MVC   H14,H9                                                           
         MVC   H4+49(9),=C'STATION -'                                           
         MVC   H4+59(9),BIGSTA                                                  
         MVC   H6+49(9),=C'REP     -'                                           
         MVC   H6+59(20),CONREP                                                 
         MVC   H5+96(16),=C'**** PAY TO ****'                                   
         MVC   H6+96(20),PANAME                                                 
         MVC   H7+96(24),PA1                                                    
         MVC   H8+96(24),PA2                                                    
         LA    RE,H8+119                                                        
HLSLIDE  CLI   0(RE),0                                                          
         BE    *+8                                                              
         CLI   0(RE),C' '                                                       
         BE    *+8                                                              
         B     *+8                                                              
         BCT   RE,HLSLIDE                                                       
         LA    RE,1(RE)                                                         
         MVI   0(RE),C','                                                       
         LA    RE,1(RE)                                                         
         MVC   0(8,RE),PA3                                                      
         MVC   H8(20),=CL20'UNCLRD ITEMS ONLY'                                  
         CLI   QOPT1,C'P'                                                       
         BNE   *+10                                                             
         MVC   H8(20),=CL20'CLEARED ITEMS ONLY'                                 
         CLI   QOPT1,C'A'                                                       
         BNE   *+10                                                             
         MVC   H8(20),=CL20'CLRD && UNCLRD ITEMS'                               
         CLI   QOPT2,C'U'                                                       
         BNE   *+10                                                             
         MVC   H8+21(12),=C'UNAFID SPOTS'                                       
         CLC   P1(40),SVDESCL                                                   
         BE    *+14                                                             
         MVC   MID1,SVDESCL                                                     
         MVI   FORCEMID,C'Y'                                                    
         L     R7,MEDBUFF                                                       
         L     R4,MEDAFRST                                                      
         LA    R3,5                                                             
         LA    R6,H13+4                                                         
MYHEAD1  CLI   0(R4),0                                                          
         BE    MYHEADX                                                          
         GOTO1 DATCON,DUB,(2,(R4)),(4,(R6))                                     
         GOTO1 DATCON,DUB,(2,2(R4)),(4,6(R6))                                   
         MVI   5(R6),C'-'                                                       
         LA    R4,12(R4)                                                        
         LA    R6,26(R6)                                                        
         BCT   R3,MYHEAD1                                                       
MYHEAD4  DS    0H                                                               
MYHEADX  XIT1                                                                   
SP60RA   DC    F'0'                                                             
SP60RB   DC    F'0'                                                             
SP60RC   DC    F'0'                                                             
SP60R2   DC    F'0'                                                             
         LTORG                                                                  
         EJECT                                                                  
RQFIRST  CSECT                                                                  
         NMOD1 0,RQFIRST                                                        
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         USING SP60WK,R2                                                        
         MVC   STACAP(7),=C'STATION'                                            
         MVC   IQREP,SPACES        REFORMAT REQUEST                             
         MVC   IQMGR,SPACES                                                     
         MVC   IQPGR,SPACES                                                     
         MVC   IQINV,SPACES                                                     
         MVC   REASTART,QSTART     SAVE REQUEST DATES                           
         CLC   QPROG,=C'U7'                                                     
         BE    *+10                                                             
         CLC   QPROG,=C'21'                                                     
         BNE   RQ1                                                              
         MVC   QAREA+8(2),SPACES                                                
         MVC   IQREP,QAREA+29                                                   
         MVC   QAREA+29(3),SPACES                                               
         MVC   IQINV,QAREA+56                                                   
         MVC   QAREA+57(10),SPACES                                              
RQ1      DS    0H                                                               
         MVC   MAXLINES,SVMAXLIN                                                
         GOTO1 VFOOT,DMCB,(RA)                                                  
         CLI   FOOT1,C' '                                                       
         BE    RQ1NOFT                                                          
         ZIC   R0,MAXLINES                                                      
         SH    R0,=H'3'                                                         
         STC   R0,MAXLINES                                                      
         MVC   FOOT1,SPACES                                                     
RQ1NOFT  DS    0H                                                               
         SPACE 2                                                                
         LA    RE,PROGPROF                                                      
         USING PROFDSCT,RE                                                      
         XC    SPOTPROF,SPOTPROF                                                
         MVI   FORCEHED,C'Y'                                                    
         MVI   PASS,0                                                           
         MVC   RTYPE,=C'RS'                                                     
         MVI   RTYPE+2,C' '                                                     
         LA    R1,H11+35                                                        
         ST    R1,AHDATES                                                       
         MVI   SPACESW,1                                                        
         L     RF,APTSDESC                                                      
         ST    RF,DDESC                                                         
         LA    RF,P1                                                            
         ST    RF,DSTAGRID                                                      
         LA    RF,P2+31                                                         
         ST    RF,PSTASPT                                                       
         LA    RF,P2+101                                                        
         ST    RF,PSTACOST                                                      
         LA    RF,P2+31                                                         
         ST    RF,PSTAGRID                                                      
         MVI   PENNYSW,1                                                        
         MVC   NUMWK,=F'56'                                                     
         LA    RE,PROGPROF                                                      
         SPACE 2                                                                
         LA    RF,DATEOPTS         SET UP OPTIONS                               
RQPR1X   MVC   DATEOPT,1(RF)                                                    
         SPACE 2                                                                
         LA    RF,SORTOPTS                                                      
RQPR2    CLI   0(RF),X'FF'         SORT OPTIONS                                 
         BE    RQPR2X                                                           
         CLC   0(1,RF),PROFSORT                                                 
         BE    RQPR2X                                                           
         LA    RF,2(RF)                                                         
         B     RQPR2                                                            
RQPR2X   MVC   SORTOPT,1(RF)                                                    
         SPACE 2                                                                
         LA    RF,FRMTOPTS                                                      
RQPR3X   MVC   FRMTOPT,1(RF)                                                    
         SPACE 2                                                                
         LA    RF,CNDSOPTS                                                      
RQPR4    CLI   0(RF),X'FF'                                                      
         BE    RQPR4X                                                           
         CLC   0(1,RF),PROFCNDS                                                 
         BE    RQPR4X                                                           
         LA    RF,2(RF)                                                         
         B     RQPR4                                                            
RQPR4X   MVC   CNDSOPT,1(RF)                                                    
         EJECT                                                                  
*        SET UP OPTON SWITCHS                                                   
         MVC   VARFRMT,DATEOPT                                                  
         MVC   SCNDDTSW,DATEOPT+1                                               
         MVC   SORTFRMT,SORTOPT                                                 
         NI    SORTFRMT,X'0F'      STRIP HOB                                    
         MVI   SORTREQ,0                                                        
         CLI   SORTFRMT,0                                                       
         BE    *+8                                                              
         MVI   SORTREQ,1                                                        
         MVI   SORTPASS,1                                                       
         MVC   LENGRID,FRMTOPT                                                  
         XC    NOINGRID,NOINGRID                                                
         MVC   NOINGRID+1(1),FRMTOPT+1                                          
         MVC   PGCNDSW,CNDSOPT                                                  
         EJECT                                                                  
         L     RF,APRSDESC                                                      
         ST    RF,DDESC                                                         
         LA    RF,P1                                                            
         ST    RF,DSTAGRID                                                      
         MVC   NUMWK,=F'56'                                                     
         XC    NOINGRID,NOINGRID                                                
         MVC   NOINGRID+1(1),FRMTOPT+2                                          
         SPACE 2                                                                
         SPACE 2                                                                
M2RPTX   DS    0H                                                               
         XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
RQFRSTA  CSECT                                                                  
         NMOD1 0,RQFRSTA                                                        
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         USING SP60WK,R2                                                        
         LA    RE,PROGPROF                                                      
         USING PROFDSCT,RE                                                      
         MVC   DETOPTS,=X'01010101'  SET DETAIL OPTIONS                         
         XC    CURRREP,CURRREP                                                  
         XC    BREP,BREP                                                        
         CLC   QREP,=C' * '                                                     
         BE    M21                                                              
         CLC   QREP,=C'   '                                                     
         BE    M21                                                              
         GOTO1 VRCPACK,DMCB,(C'P',QREP),BREP                                    
*                                                                               
M21      MVC   SUMOPTS,=X'010101'  SET SUMMARY OPTIONS                          
M2AA     DS    0H                                                               
         MVI   SUBPSW,0                                                         
         MVC   MSOPT,SPACES        INITIALIZE MS OPTIONS                        
         MVC   SVOPTS,QOPT1        SAVE REPORT OPTIONS                          
         MVC   SUBPROG1,=C'M2'                                                  
COMPOK   DS    0H                                                               
         SPACE 2                                                                
         MVC   SUBPROG2,=C'01'                                                  
         GOTO1 REPCALOV,DMCB,(RA),SUBPROG                                       
         L     RE,DMCB+4                                                        
         ST    RE,SVPH01                                                        
         MVC   SUBPROG2,=C'02'                                                  
         GOTO1 REPCALOV,DMCB,(RA),SUBPROG                                       
         L     RE,DMCB+4                                                        
         ST    RE,SVPH02                                                        
         MVC   SUBPROG2,=C'04'                                                  
         GOTO1 REPCALOV,DMCB,(RA),SUBPROG                                       
         MVC   SVPH04,DMCB+4                                                    
         L     RF,VSVMDBLK                                                      
         L     RE,MEDBUFF                                                       
         LA    R1,980                                                           
         MOVE  ((RF),(R1)),(RE)                                                 
         XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
EFRSTC   CSECT                                                                  
         NMOD1 0,EFRSTC                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         USING SP60WK,R2                                                        
         MVC   QSTART(12),REASTART   RESTORE REQUEST DATES                      
         MVC   PASSQST(12),QSTART                                               
         MVC   PASSTAB(12),QSTART                                               
* SET NUMBER OF LEVELS                                                          
         LA    RF,LVCNTRL                                                       
         LA    RE,5                                                             
         NI    0(RF),X'7F'                                                      
         LA    RF,1(RF)                                                         
         BCT   RE,*-8                                                           
         L     RF,BUFFBUFF                                                      
         USING BUFFALOD,RF                                                      
         L     RE,BUFFROWS                                                      
         BCTR  RE,0                                                             
         MH    RE,=H'4'                                                         
         LA    RE,LVCNTRL(RE)                                                   
         OI    0(RE),X'80'                                                      
         DROP  RF                                                               
         SPACE 2                                                                
* CREATE WEEKLY TABLES FOR ALL REPORTS                                          
         MVI   ALLOWCWM,C'Y'                                                    
         MVC   SVRCPROG,RCPROG     SAVE THIS PROGRAM CODE                       
         MVI   SPOTPROF+2,0        FORCE BROADCAST MONTHS                       
         XC    SPOTPROF+6(3),SPOTPROF+6 FORCE MON. START                        
         CLI   QMED,C'N'           EXCEPT FOR NETWORK                           
         BNE   *+8                                                              
         MVI   SPOTPROF+2,2        WHICH GETS CALENDAR MONTHS                   
         MVC   MEDNUMWK,=F'56'     CREATE BTS TABLES                            
         MVC   MEDNUMMO,=F'12'                                                  
         MVC   MEDNUMQT,=F'4'                                                   
         MVC   MEDNUMPE,=F'1'                                                   
         MVC   RCPROG,SVRCPROG                                                  
EFRSTA   DS    0H                                                               
         GOTO1 MEDDATE,DMCB,(RA)                                                
         MVC   RCPROG,SVRCPROG     RESET RCPROG                                 
         MVC   SVRDTE,MEDPERD      SAVE REQUEST DATES                           
         MVI   PASS,0                                                           
         MVI   MAXPASS,1                                                        
         CLI   VARFRMT,0           FIXED FORMAT                                 
         BNE   EFRSTX               NO - EXIT                                   
         LA    RE,1                                                             
         LA    R6,PASSTAB                                                       
         L     R9,MEDAFRST                                                      
SETPASS  STC   RE,MAXPASS          SAVE HIGHEST PASS                            
         LH    R8,NOINGRID         SET TO NUMBER OF WEEKS IN PASS               
         GOTO1 DATCON,DMCB,(X'02',(R9)),(X'00',0(R6))                           
SETPASS1 GOTO1 DATCON,DMCB,(X'02',2(R9)),(X'00',6(R6))                          
SETPASS2 LA    R9,12(R9)                                                        
         C     R9,MEDALAST                                                      
         BH    SETPASSX                                                         
         CLI   0(R9),0                                                          
         BE    SETPASS2                                                         
         BCT   R8,SETPASS1                                                      
         ZIC   RE,MAXPASS                                                       
         LA    RE,1(RE)            BUMP MAXPASS                                 
         LA    R6,12(R6)           BUMP DATE SAVE                               
         B     SETPASS                                                          
SETPASSX MVC   PASSQST(12),PASSTAB                                              
EFRSTX   MVC   QSTART(12),PASSQST                                               
         XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
GETSADDR CSECT                                                                  
         NMOD1 0,GETSADDR                                                       
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         USING SP60WK,R2                                                        
         MVC   SAKYSAVE,KEY                                                     
         LA    R6,KEY                                                           
         USING ADDRREC,R6                                                       
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   ADDKTYPE,C'A'                                                    
         MVC   ADDKMED,QMED                                                     
         MVC   ADDKCALL,STA                                                     
         CLI   ADDKCALL+4,C' '                                                  
         BNE   *+10                                                             
         MVC   ADDKCALL+4(1),QMED                                               
         CLI   QMED,C'C'                                                        
         BNE   *+8                                                              
         MVI   ADDKCALL+4,C'C'                                                  
         MVC   ADDKAGY,AGY                                                      
         GOTO1 HIGHSTAD                                                         
         L     R6,ADSTATAD                                                      
         CLC   KEY(9),0(R6)                                                     
         BE    M80                                                              
         MVC   ANAME(86),SPACES                                                 
         MVC   ANAME(20),=CL20'* ADDRESS UNKNOWN *'                             
         B     M80A                                                             
M80      DS    0C                                                               
         L     R6,ADSTATAD                                                      
         LA    RF,A3LINE                                                        
         SH    RF,=H'4'                                                         
M84CAN   CLI   0(RF),0                                                          
         BE    *+8                                                              
         CLI   0(RF),C' '                                                       
         BNE   *+10                                                             
         BCTR  RF,0                                                             
         B     M84CAN                                                           
         LA    RF,1(RF)                                                         
         MVI   0(RF),C','                                                       
         MVC   1(2,RF),A3LINE                                                   
         MVC   A3LINE(L'ABIGZIP),ABIGZIP                                        
M80A     MVC   PANAME,ANAME                                                     
         MVC   PA1,A1LINE                                                       
         MVC   PA2,A2LINE                                                       
         MVC   PA3,A3LINE                                                       
         L     R6,ADSTAT                                                        
         USING STAREC,R6                                                        
         XC    PAYREP,PAYREP       GET REP NAMES                                
         MVC   PAYREP(06),=C'DIRECT'                                            
         TM    SPAYREP,X'F0'                                                    
         BNO   M81                                                              
         CLC   SPAYREP,=C'000'     TEST FOR VALID REP                           
         BE    M81                                                              
         MVC   WORK(3),SPAYREP                                                  
         GOTO1 VGETREP                                                          
         L     RE,ADSTATAD                                                      
         USING ADDRREC,RE                                                       
         MVC   PAYREP,ANAME                                                     
         MVC   PANAME,ANAME                                                     
         MVC   PA1,A1LINE                                                       
         MVC   PA2,A2LINE                                                       
         MVC   PA3,A3LINE                                                       
M81      XC    CONREP,CONREP                                                    
         MVC   CONREP(4),=C'NONE'                                               
         TM    SCONREP,X'F0'       TEST FOR VALID REP NUM                       
         BNO   M84                                                              
         CLC   SCONREP,=C'000'                                                  
         BE    M84                                                              
         MVC   WORK(3),SCONREP                                                  
         GOTO1 VGETREP                                                          
         L     RE,ADSTATAD                                                      
         MVC   CONREP,ANAME                                                     
         DROP  RE                                                               
M82      GOTO1 VGETREP             GET REP ADDRESS                              
M84      DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
M8EXIT   MVC   KEY,SAKYSAVE                                                     
         XMOD1 1                                                                
         DROP  R5                                                               
         LTORG                                                                  
         DROP  R6                                                               
SAKYSAVE DS    CL32                                                             
         EJECT                                                                  
         EJECT                                                                  
GETGL    CSECT                                                                  
         NMOD1 0,GETGOAL                                                        
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         USING SP60WK,R2                                                        
         USING MEDBLOCK,R7                                                      
         USING MEDDATA,R4                                                       
         USING SUMDSECT,R3                                                      
GOALX    XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
GETCAP   CSECT                                                                  
         NMOD1 0,GETCAP                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         USING SP60WK,R2                                                        
         L     R5,ADBUY                                                         
         LA    R5,24(R5)                                                        
         USING PKGELEM,R5                                                       
         XC    PKGAREA,PKGAREA                                                  
GETCAP1  CLI   PKGCODE,5                                                        
         BE    GETCAP2                                                          
         CLI   PKGCODE,0                                                        
         BE    GETCAPX                                                          
         SR    RE,RE                                                            
         IC    RE,PKGLEN                                                        
         AR    R5,RE                                                            
         B     GETCAP1                                                          
         SPACE 2                                                                
GETCAP2  CLI   PKGIND,1                                                         
         BNE   *+14                                                             
         MVC   PKGAREA(7),=C'PKG MST'                                           
         B     GETCAPX                                                          
         CLI   PKGIND,3                                                         
         BNE   *+14                                                             
         MVC   PKGAREA(7),=C'ORB MST'                                           
         B     GETCAPX                                                          
         CLI   PKGIND,5                                                         
         BNE   *+14                                                             
         MVC   PKGAREA(7),=C'REV MST'                                           
         B     GETCAPX                                                          
         CLI   PKGIND,7                                                         
         BE    GETCAPX                                                          
         CLI   PKGIND,8                                                         
         BNE   GETCAP3                                                          
         MVC   PKGAREA(4),=C'*MG*'                                              
         B     *+10                                                             
GETCAP3  MVC   PKGAREA(4),=C'MST='                                              
         SR    R0,R0                                                            
         IC    R0,PKGLINES                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PKGAREA+4(3),DUB+6(2)                                            
         CLI   PKGIND,8                                                         
         BNE   GETCAPX                                                          
         DROP  R5                                                               
         L     R5,ADBUY                                                         
         LA    R5,24(R5)                                                        
         USING BDELEM,R5                                                        
         GOTO1 DATCON,DMCB,(X'02',BDMGDATE),(X'08',PKGAREA+8)                   
GETCAPX  DS    0H                                                               
         SPACE 2                                                                
         EJECT                                                                  
         EJECT                                                                  
GETCOM   L     R5,ADBUY                                                         
         LA    R5,24(R5)                                                        
         USING COMELEM,R5                                                       
         LA    RE,COMAREA                                                       
         LA    RF,400                                                           
         XCEF                                                                   
GETCOM2  CLI   CMCODE,0                                                         
         BE    GETCOMX                                                          
         CLI   CMCODE,X'66'                                                     
         BE    GETCOM4                                                          
GETCOM3  SR    R0,R0                                                            
         IC    R0,CMLEN                                                         
         AR    R5,R0                                                            
         B     GETCOM2                                                          
*                                                                               
GETCOM4  LA    R7,PROGPROF                                                      
         USING PROFDSCT,R7                                                      
         LA    R4,COMAREA                                                       
         CLI   PROFCOM,C'Y'        PRINT ALL COMMENTS                           
         BE    GETCOM5                                                          
         DROP  R7                                                               
         CLI   CMNUM,4                                                          
         BL    GETCOM3                                                          
         CLI   CMNUM,5             GET COMMENT SLOT                             
         BH    GETCOM3                                                          
GETCOM5  SR    R7,R7                                                            
         IC    R7,CMNUM                                                         
         BCTR  R7,0                                                             
         MH    R7,=H'80'                                                        
         AR    R4,R7                                                            
         SR    R7,R7                                                            
         IC    R7,CMLEN                                                         
         SH    R7,=H'4'                                                         
         LTR   R7,R7                                                            
         BM    GETCOM3                                                          
         EX    R7,*+8                                                           
         B     GETCOM3                                                          
         MVC   0(0,R4),CMDATA                                                   
GETCOMX  XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
COMPRNT  CSECT                                                                  
         NMOD1 0,COMPRNT                                                        
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         USING SP60WK,R2                                                        
         LA    R4,COMAREA                                                       
         LA    R5,5                                                             
         L     R6,FULL                                                          
         CLI   0(R6),0                                                          
         BNE   *+8                                                              
         MVI   0(R6),C' '                                                       
         CLC   1(50,R6),0(R6)                                                   
         BE    COMPRNT1                                                         
         LA    R6,132(R6)                                                       
         B     *-14                                                             
COMPRNT1 OC    0(76,R4),0(R4)                                                   
         BZ    *+14                                                             
         MVC   0(76,R6),0(R4)                                                   
         LA    R6,132(R6)                                                       
         LA    R4,80(R4)                                                        
         BCT   R5,COMPRNT1                                                      
         CLI   COMAREA,0                                                        
         BE    *+8                                                              
         MVI   0(R6),0                                                          
         XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
GETREP   CSECT                                                                  
         NMOD1 0,GETREP                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         USING SP60WK,R2                                                        
         MVC   RPKYSAVE,KEY                                                     
         LA    R6,KEY                                                           
         USING REPREC,R6                                                        
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   REPKTYPE,C'R'                                                    
         MVC   REPKMED,QMED                                                     
         MVC   REPKREP,WORK                                                     
         MVC   REPKAGY,AGY                                                      
         GOTO1 READREP                                                          
         MVC   KEY,RPKYSAVE                                                     
         L     R6,ADREP                                                         
         L     RE,ADSTATAD                                                      
         USING ADDRREC,RE                                                       
         MVC   ANAME,RNAME                                                      
         MVC   A1LINE,R1LINE                                                    
         MVC   A2LINE,R2LINE                                                    
         MVC   A3LINE(8),R3LINE                                                 
         MVC   ABIGZIP,RBIGZIP                                                  
         XMOD1 1                                                                
         DROP  R6                                                               
         DROP  RE                                                               
         LTORG                                                                  
RPKYSAVE DS    CL32                                                             
         EJECT                                                                  
STATOTC  CSECT                                                                  
         NMOD1 0,STATOT                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         USING SP60WK,R2                                                        
         USING MEDBLOCK,R7                                                      
         USING MEDDATA,R4                                                       
         USING SUMDSECT,R3                                                      
         MVC   MEDNUMWK,NUMWK      SET UP FOR MEDDATE                           
         MVC   MEDNUMMO,=F'13'                                                  
         MVC   MEDNUMQT,=F'4'                                                   
         MVC   MEDNUMPE,=F'1'                                                   
         MVC   MEDLCHNK,=F'188'                                                 
         MVC   QSTART(12),PASSQST  SET PASS START AND END                       
         GOTO1 MEDDATE,DMCB,(RA)                                                
         MVC   QSTART(12),REASTART RESTORE REQUEST START AND END                
         MVC   P(13),=C'STATION TOTAL'                                          
         MVC   P+21(5),=C'SPOTS'                                                
STATOT1  CLI   BUYACT,0                                                         
         BNE   STATOT1A                                                         
         OC    STASPOT,STASPOT                                                  
         BNZ   *+14                                                             
         OC    STACOST,STACOST                                                  
         BZ    M10B53                                                           
STATOT1A ZIC   RE,MAXLINES                                                      
         ZIC   RF,LINE                                                          
         SR    RE,RF                                                            
         CH    RE,=H'5'                                                         
         BH    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         MVC   P+31(13),=C'ORDERED GROSS'                                       
         MVC   P+49(11),=C'ORDERED NET'                                         
         MVC   P+65(11),=C'CLEARED NET'                                         
         MVC   P+86(16),=C'   UNCLEARED NET'                                    
         CLC   QPROG(2),=C'I3'                                                  
         BE    STATOT2                                                          
         MVC   P+65(13),=C'INVOICE   NET'                                       
         MVC   P+86(16),=C'DIFFERENCE   NET'                                    
STATOT2  DS    0H'0'                                                            
         MVI   P2,0                                                             
         EDIT  STASPOT,(5,P3+21)                                                
         EDIT  STACOST,(14,P3+30),2,MINUS=YES,COMMAS=YES                        
         EDIT  STACOSTN,(14,P3+46),2,MINUS=YES,COMMAS=YES                       
         EDIT  STACINV,(14,P3+64),2,MINUS=YES,COMMAS=YES                        
         L     RF,STACOSTN                                                      
         S     RF,STACINV                                                       
         EDIT  (RF),(14,P3+88),2,MINUS=YES,COMMAS=YES                           
M10B52   GOTO1 REPORT                                                           
         GOTO1 VFOOT,DMCB,(RA)                                                  
         CLI   PASS,X'FF'                                                       
         BE    M10B54                                                           
         CLI   MAXPASS,1           ONLY ONE PASS                                
         B     M10B54              THIS WILL DISALLOW WRAPAROUND                
         BE    M10B54                                                           
         SPACE 2                                                                
* SET UP FOR NEXT PASS                                                          
M10B53   CLI   MAXPASS,1                                                        
         B     M10B54              DISALLOW WRAPAROUND                          
         BE    M10B54                                                           
         CLI   PASS,X'FF'                                                       
         BE    M10B54                                                           
         SR    RE,RE                                                            
         IC    RE,PASS                                                          
         LA    RE,1(RE)                                                         
         STC   RE,PASS                                                          
         CLC   PASS,MAXPASS        TRUE END OF STATION                          
         BL    M10B6                                                            
         MVI   PASS,X'FF'                                                       
         XC    STAGRID(56),STAGRID                                              
         MVC   STASPOT(44),STTSPOT                                              
         XC    STTSPOT(44),STTSPOT                                              
         MVC   MEDPERD,SVRDTE                                                   
         B     STATOT1                                                          
M10B54   MVI   PASS,0              YES - RESET AND EXIT                         
         MVC   WORK(12),PASSTAB                                                 
         MVC   PASSQST(12),PASSTAB                                              
         MVC   MID1,SPACES                                                      
         MVC   P1,SPACES                                                        
         MVC   P2,SPACES                                                        
         MVC   P3,SPACES                                                        
         XC    STAGRID(56),STAGRID                                              
         XC    STASPOT,STASPOT                                                  
         XC    STACOSTN,STACOSTN                                                
         XC    STADEMS(32),STADEMS                                              
         XC    STACOST(8),STACOST                                               
         B     M10EXIT                                                          
M10B6    MVI   MODE,REREAD                                                      
         SR    R6,R6                                                            
         IC    R6,PASS                                                          
         MH    R6,=H'12'                                                        
         LA    R6,PASSTAB(R6)                                                   
         USING PASSTABD,R6                                                      
         MVC   WORK(12),0(R6)                                                   
         MVC   PASSQST(12),0(R6)   SET PASS START AND END                       
         XC    STAGRID(56),STAGRID                                              
         XC    STASPOT,STASPOT                                                  
         XC    STACOSTN,STACOSTN                                                
         XC    STADEMS(32),STADEMS                                              
         XC    STACOST(8),STACOST                                               
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         IC    RE,LINE                                                          
         IC    RF,MAXLINES                                                      
         SR    RF,RE                                                            
         L     R1,AHDATES                                                       
         CLI   FORCEHED,C'Y'                                                    
         BE    M10B7                                                            
         MVI   FORCEHED,C'Y'                                                    
         C     RF,=F'14'                                                        
         BL    M10EXIT                                                          
         MVI   FORCEHED,C'N'                                                    
         L     R1,PSTAGRID                                                      
M10B7    GOTO1 VHDATES                                                          
         MVI   ALLOWLIN,14                                                      
         MVI   FORCEMID,C'Y'                                                    
M10EXIT  GOTO1 DATCON,DMCB,WORK,(X'03',PASSSD3)                                 
         GOTO1 DATCON,DMCB,WORK+6,(X'03',PASSED3)                               
         ZIC   RE,MAXLINES                                                      
         ZIC   RF,LINE                                                          
         SR    RE,RF                                                            
         C     RE,=F'8'                                                         
         BH    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
         EJECT                                                                  
MLASTC   CSECT                                                                  
         NMOD1 0,MALSTC                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         USING SP60WK,R2                                                        
         MVC   WEIGHT,SPWEIGHT                                                  
         L     R8,BUFFBUFF                                                      
         MVC   DMCB+8(20),LVCNTRL                                               
         GOTO1 BUFFALO,DMCB,=C'ADD',(X'90',(R8))                                
         GOTO1 BUFFALO,DMCB,=C'ADD',(X'91',(R8))                                
         GOTO1 BUFFALO,DMCB,=C'CLEAR',(X'90',(R8)),(X'80',1)                    
         GOTO1 BUFFALO,DMCB,=C'CLEAR',(X'91',(R8)),(X'80',1)                    
         XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
EXTRCT   CSECT                                                                  
         NMOD1 0,EXTRACT                                                        
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         USING SP60WK,R2                                                        
         USING MEDBLOCK,R7                                                      
         USING MEDDATA,R4                                                       
         USING SUMDSECT,R3                                                      
         MVC   QSTART(12),PASSQST                                               
         MVC   MEDNUMWK,NUMWK                                                   
         MVC   MEDNUMMO,=F'13'                                                  
         MVC   MEDNUMQT,=F'4'                                                   
         MVC   MEDNUMPE,=F'1'                                                   
         MVC   MEDLCHNK,=F'188'                                                 
         GOTO1 MEDDATE,DMCB,(RA)                                                
         MVC   MEDBRAND,BPRD                                                    
         CLC   QPRD2,=C'   '                                                    
         BE    *+8                                                              
         MVI   MEDBRAND,X'FF'                                                   
         MVC   BYTE,MEDSPTLN                                                    
         MVI   MEDSPTLN,0                                                       
         MVI   MEDEXTDM,4                                                       
         L     R5,ADBUY                                                         
         USING BUYREC,R5                                                        
         MVC   HLDBDTIM,BDTIME                                                  
         MVI   BDTIME,0                                                         
         SPACE 2                                                                
         SPACE 2                                                                
         LA    R5,2                SET DEMO LOOKUP CODE                         
         CLI   QRERATE,C' '                                                     
         BE    EXTRCT2                                                          
         CLI   QRERATE,C'A'        ADJUST ONLY                                  
         BNE   EXTRCT1                                                          
         LA    R5,5                                                             
         B     EXTRCT2                                                          
EXTRCT1  LA    R5,3                SET FOR PURCHASED RERATED                    
         CLC   QHUT1,=C'NO'                                                     
         BE    *+8                                                              
         LA    R5,1(R5)            SET FOR ADJUSTMENTS                          
         CLI   QRERATE,C'I'        SET FOR AFFID RERATE                         
         BNE   *+8                                                              
         LA    R5,3(R5)                                                         
*                                                                               
EXTRCT2  GOTO1 MEDGETBY,DMCB,(RA),(R5)                                          
         L     R5,ADBUY                                                         
         USING BUYREC,R5                                                        
         MVC   BYTE,BDSEC                                                       
         MVC   BDTIME,HLDBDTIM                                                  
         DROP  R5                                                               
         BAS   RE,GETHP                                                         
         XC    PRTLINE,PRTLINE                                                  
         LA    R3,PRTLINE                                                       
         L     R5,MEDAFRST         RELEASE DATA TO BUFFALO                      
         CLI   MRPTTYP,C'2'        RPT 2 DOESNT NEED WEEKLYS                    
         BNE   EXTRCT3                                                          
         LA    R5,MEDMON01                                                      
EXTRCT3  MVI   SUMCODE,X'90'                                                    
         MVC   SUMDPGNO(8),MEDDPGNO     SET DAYPART                             
         MVC   SUMSLN,BYTE                                                      
         MVC   MEDSPTLN,BYTE                                                    
         L     R4,4(R5)                                                         
         MVI   SUMRTYP,1                                                        
         LA    RF,MEDPERD                                                       
         CR    R5,RF               END                                          
         BH    EXTRCTX                                                          
         BNE   EXTRCT3A            WEEKLY OR MONTHLY                            
         OC    MEDBYD(12),MEDBYD                                                
         BZ    EXTRCT5              NO - EXIT                                   
         MVI   SUMRTYP,3                                                        
         MVC   SUMDT,=X'FFFFFFFF'                                               
         B     EXTRCT4                                                          
EXTRCT3A OC    0(4,R5),0(R5)       ACTIVE SLOT                                  
         BZ    EXTRCT5                                                          
         LA    RF,MEDMON01                                                      
         CR    R5,RF               MONTHLY                                      
         BL    EXTRCT3B                                                         
         MVI   SUMRTYP,2            YES-SET RECORD CODE                         
         CLI   MRPTTYP,C'2'        MONTHLYS FOR RPT 2 ONLY                      
         BE    EXTRCT3B                                                         
         LA    R5,MEDPERD                                                       
         B     EXTRCT3                                                          
EXTRCT3B MVC   SUMDT,0(R5)                                                      
EXTRCT4  LA    RE,SUMKEY           SET UP DATA ITEM DISPLACEMENTS               
         USING SUMDATA,RE                                                       
         L     RF,BUFFBUFF                                                      
         USING BUFFALOD,RF                                                      
         A     RE,BUFFLKEY                                                      
         XC    SUMDATA,SUMDATA                                                  
         MVC   SUMSPOTS,MEDBYSPT   MOVE IN DATA                                 
         MVC   SUMDL,MEDBYD                                                     
         MVC   SUMDLEQ,MEDBYDEQ                                                 
         MVC   SUMD1,MEDBY1                                                     
         MVC   SUMD1EQ,MEDBY1EQ                                                 
         MVC   SUMD2,MEDBY2                                                     
         MVC   SUMD2EQ,MEDBY2EQ                                                 
         MVC   SUMD3,MEDBY3                                                     
         MVC   SUMD3EQ,MEDBY3EQ                                                 
         MVC   SUMD4,MEDBY4                                                     
         MVC   SUMD4EQ,MEDBY4EQ                                                 
         LA    R8,PROGPROF                                                      
         USING PROFDSCT,R8                                                      
EXTRCT4T L     R8,BUFFBUFF                                                      
         MVC   SUMDPGNO(9),=9X'FF'                                              
         MVI   SUMCODE,X'90'                                                    
         BAS   R9,EXTPUT                                                        
         MVI   SUMCODE,X'91'                                                    
         BAS   R9,EXTPUT                                                        
* GET NEXT BUFFALO ITEM                                                         
EXTRCT5  LA    R5,12(R5)                                                        
         B     EXTRCT3                                                          
EXTPUT   CLI   SUMCODE,X'91'                                                    
         BER   R9                                                               
         OC    SUMSPOTS(12),SUMSPOTS                                            
         BZR   R9                                                               
         GOTO1 BUFFALO,DMCB,=C'PUT',(R8),PRTLINE                                
         BR    R9                                                               
         SPACE 2                                                                
EXTRCTX  XMOD1 1                                                                
         DROP  RE                                                               
         EJECT                                                                  
GETHP    NTR1                                                                   
         MVI   MGSW,0                                                           
         L     R7,MEDBUFF                                                       
         USING MEDBLOCK,R7                                                      
         L     R3,ADBUY                                                         
         USING BUYREC,R3                                                        
         L     R5,MEDAFRST                                                      
         MVC   STRDTE,0(R5)                                                     
         L     R5,MEDALAST                                                      
         MVC   ENDDTE,2(R5)                                                     
         XC    CSPNO,CSPNO                                                      
         MVC   CSPNO(1),BDDAY                                                   
         OC    PGNOENT,PGNOENT                                                  
         BNZ   GETHP1                                                           
         XC    HPSNO,HPSNO                                                      
         MVC   LASTGSLT,VPGRID                                                  
         XC    PGNOENT,PGNOENT                                                  
         XC    PIGBRND,PIGBRND                                                  
GETHP1   LA    R5,BDELEM                                                        
         USING REGELEM,R5                                                       
REGNXT   ZIC   RE,RLEN                                                          
         AR    R5,RE                                                            
         CLI   0(R5),0                                                          
         BE    GETHPX                                                           
         CLI   0(R5),4             BRAND PIGGYBACK ELEMENT                      
         BNE   BRPBX                                                            
         LR    RE,R5                                                            
         USING PBELEM,RE                                                        
         MVC   PIGBRND,PBPROD                                                   
         CLC   PIGBRND,BPRD        BYPASS IF REQUESTED BRAND                    
         BE    GETHPX               IS PASSIVE                                  
         DROP  RE                                                               
BRPBX    DS    0H'0'                                                            
         CLI   RCODE,6                                                          
         BL    REGNXT                                                           
         CLI   RCODE,13                                                         
         BH    REGNXT                                                           
         CLC   RDATE,STRDTE                                                     
         BL    REGNXT                                                           
         CLC   RDATE,ENDDTE                                                     
         BH    REGNXT                                                           
         CLI   RCODE,9                                                          
         BH    *+20                                                             
         CLI   RCODE,7             FORCE PRINTING OF BRAND MODE                 
         BE    *+12                 PREEMPTS                                    
         TM    RSTATUS,X'80'                                                    
         BO    REGNXT                                                           
         ST    R5,SPOTADDR                                                      
         MVI   SPOTYORN,C'Y'                                                    
         GOTO1 SPOTHOOK                                                         
         CLI   SPOTYORN,C'N'                                                    
         BE    REGNXT                                                           
         ZIC   RE,HPSNO                                                         
         LA    RE,1(RE)                                                         
         STC   RE,HPSNO                                                         
         CLI   RCODE,11            BRAND ELEMENT                                
         BL    GETHP2                                                           
         CLI   RLEN,14             UNALLOCATED                                  
         BL    REGNXT               BYPASS                                      
         CLI   MEDBRAND,X'FF'      POL PRODUCT                                  
         BE    GETHP2               OK                                          
         CLC   MEDBRAND,RPPRD      WRONG BRAND                                  
         BNE   REGNXT              BYPASS                                       
         SPACE 2                                                                
GETHP2   LA    R8,WORK                                                          
         USING PGDWK,R8                                                         
         USING PGSORT,R8                                                        
         ZIC   R0,1(R5)                                                         
         LR    RF,R5                                                            
         AR    RF,R0                                                            
         XC    WORK,WORK                                                        
         CLI   0(RF),X'10'                                                      
         BNE   *+16                                                             
         USING AFFELEM,RF                                                       
         MVC   PGDAFDAY,ADATE                                                   
         MVC   PGDAFTIM,ATIME                                                   
         DROP  RF                                                               
         MVC   PGDFDAY,CSPNO                                                    
         MVC   PGDCUTIN+1(3),BDCOST                                             
         MVI   PGDCUTIN,0                                                       
         TM    RSTATUS,X'20'                                                    
         BZ    *+10                                                             
         MVC   PGDCUTIN+1(3),RPCOST                                             
         MVC   PGDWK,RDATE                                                      
         ST    R5,FULL                                                          
         MVC   PGDELAD,FULL        SAVE ELEMENT ADDRESS                         
         TM    BDSTAT,X'80'        POL RADIO                                    
         BZ    *+8                                                              
         NI    PGDCUTIN+1,X'03'    CLEAR OUT SPOTS                              
*                                  BYPASS HIATUS SPOTS                          
GETHP2A0 DS    0H                                                               
*        CLI   SORTREQ,0                                                        
*        BE    *+10                                                             
*        XC    PGDELAD,PGDELAD                                                  
         MVC   PGD2BRN,PIGBRND                                                  
         CLI   RCODE,9             BRAND ELEMENT                                
         BH    GTHP22               NO                                          
         MVC   PGDSBRN,BPRD         YES                                         
         MVC   PGDNOSP,RNUM                                                     
         CLI   RCODE,7             IS THIS ELEM AN OTO                          
         BNE   *+16                 NO - DO NORMAL PROCESSING                   
         TM    6(R5),X'80'         IS IT A MINUS OTO                            
         BO    GETHP2A              YES - DONT CHECK FOR '-' TO A '-'           
         B     *+12                +OTO PROCESS AS NORMAL                       
         CLI   RCODE,6                                                          
         BNE   GETHP2A                                                          
         LR    R1,R5                                                            
GTHP2    ZIC   R0,1(R1)            CHECK FOR BRAND -OTO                         
         AR    R1,R0                                                            
         CLI   0(R1),6             END OF REGELEMS                              
         BL    GETHP2A                                                          
         CLI   0(R1),9                                                          
         BH    GETHP2A                                                          
         CLC   RDATE,2(R1)         SAME DATE                                    
         BNE   GETHP2A              NO - END OF THIS SPOT                       
         TM    6(R1),X'80'         CHECK STATUS BIT                             
         BZ    GTHPPOTO                                                         
         ZIC   RE,PGDNOSP          FOUND ONE                                    
         ZIC   RF,7(R1)            GET NUMBER OF SPOTS                          
         SR    RE,RF                                                            
         STC   RE,PGDNOSP                                                       
         LPR   RE,RE                                                            
         BZ    REGNXT                                                           
         B     GTHP2                                                            
GTHPPOTO LR    R5,R1               ADD IN +OTO SPOTS                            
         ZIC   RF,PGDNOSP                                                       
         ZIC   RE,7(R1)                                                         
         AR    RE,RF                                                            
         STC   RE,PGDNOSP                                                       
         B     GTHP2                                                            
GTHP22   DS    0H'0'                                                            
         MVC   PGDSBRN,RPPRD                                                    
         MVI   PGDNOSP,1                                                        
         TM    BDSTAT,X'80'                                                     
         BZ    GPHP1                                                            
         ZIC   R1,RPCOST                                                        
         SRL   R1,2                                                             
         STC   R1,PGDNOSP                                                       
GPHP1    CLI   RCODE,X'0C'         IS THIS ELEMENT AN OTO                       
         BNE   *+16                 NO - DO NORMAL PROCESSING                   
         TM    RSTATUS,X'80'       IS IT A MINUS OTO                            
         BO    GETHP2A1             YES - DONT CHECK FOR '-' TO A '-'           
         B     *+12                +OTO PROCESS AS NORMAL                       
         CLI   RCODE,X'0B'                                                      
         BNE   GETHP2A1                                                         
         LR    R1,R5                                                            
         MVC   STAT1,RSTATUS                                                    
         NI    STAT1,B'00100000'                                                
GPHP2    ZIC   R0,1(R1)            CHECK FOR POL -OTO                           
         AR    R1,R0                                                            
         TM    BDSTAT,X'80'        TEST BRAND POL                               
         BO    *+12                                                             
         CLI   0(R1),X'0B'         TEST NEXT FOR ORIGINAL                       
         BE    GETHP2A1            YES END OF THIS REGELEM                      
         CLI   0(R1),X'0B'         CHECK FOR END OF REGELEMS                    
         BL    GETHP2A1                                                         
         CLI   0(R1),X'0D'                                                      
         BH    GETHP2A1                                                         
*        TM    RSTATUS,X'20'       FORCE BREAK ON NEW                           
*        BO    GETHP2A1            COST OVERRIDE                                
         MVC   STAT2,6(R1)                                                      
         NI    STAT2,B'00100000'                                                
         CLC   STAT1,STAT2                                                      
         BNE   GETHP2A1                                                         
         CLC   RDATE,2(R1)         SAME DATE                                    
         BNE   GETHP2A1             NO - END OF THIS SPOT                       
         TM    6(R1),X'80'         CHECK STATUS BIT                             
         BZ    GPHPPOTO                                                         
         ZIC   RE,PGDNOSP                                                       
         LA    RF,1                SET UP NUMBER OF SPOTS                       
         TM    BDSTAT,X'80'                                                     
         BZ    GPHP3                                                            
         ZIC   RF,7(R1)                                                         
         SRL   RF,2                                                             
GPHP3    SR    RE,RF                                                            
         STC   RE,PGDNOSP                                                       
         LTR   RE,RE                                                            
         BZ    REGNXT                                                           
         B     GPHP2                                                            
GPHPPOTO LR    R5,R1               ADD IN +OTO SPOTS                            
         ZIC   RF,PGDNOSP                                                       
         LA    RE,1                                                             
         TM    BDSTAT,X'80'                                                     
         BZ    GPHP4                                                            
         ZIC   RE,7(R5)                                                         
         SRL   RE,2                                                             
GPHP4    AR    RE,RF                                                            
         STC   RE,PGDNOSP                                                       
         B     GPHP2                                                            
GETHP2A1 CLI   RLEN,14                                                          
         BNH   *+10                                                             
         MVC   PGD2BRN,RPPRD+4                                                  
         CLI   RLEN,13                                                          
         BH    GETHP2A                                                          
         MVI   PGDSBRN,X'FF'                                                    
         MVI   PGDSSLN,0                                                        
         MVC   PGDSNO,HPSNO                                                     
         B     GETHP2C                                                          
         SPACE 2                                                                
GETHP2A  TM    RSTATUS,X'04'                                                    
         BZ    GETHP2B                                                          
         MVI   PGDSBRN,X'FF'                                                    
         MVI   PGDSSLN,0                                                        
         MVC   PGDSNO,HPSNO                                                     
         MVI   PGDIND,X'08'        SET HIATUS INDICATOR                         
         B     GETHP2C                                                          
         SPACE 2                                                                
GETHP2B  TM    RSTATUS,X'40'                                                    
         BZ    *+8                                                              
         MVI   PGDIND,X'04'        SET PREEMPT INDICATOR                        
         TM    RSTATUS,X'42'                                                    
         BNO   *+8                                                              
         MVI   PGDIND,X'01'        SET MISSED INDICATOR                         
         TM    RSTATUS,X'80'                                                    
         BNO   *+8                                                              
         MVI   PGDIND,X'08'                                                     
         CLI   RCODE,9             BRAND ELEMENT                                
         BL    *+10                                                             
         MVC   PGDSBRN,RPPRD                                                    
         MVC   PGDSSLN,RPTIME                                                   
         MVC   PGDSNO,HPSNO                                                     
         SPACE 2                                                                
GETHP2C  L     RE,LASTGSLT         SET TO LAST GRID SLOT                        
         ZIC   R9,PGDNOSP                                                       
         LTR   R9,R9               BYPASS ZERO SPOTS ON POL RADIO               
         BZ    REGNXT                                                           
         BNM   *+6                                                              
         DC    H'0'                                                             
         MVI   PGDNOSP,1                                                        
GETHP2D  DS    0C                                                               
         L     RF,PGNOENT                                                       
         LA    RF,1(RF)                                                         
         ST    RF,PGNOENT                                                       
         STC   RF,PGLSLOT                                                       
         MVC   0(PGDLEN,RE),WORK                                                
         ZIC   R1,HPSNO                                                         
         LA    R1,1(R1)                                                         
         STC   R1,HPSNO                                                         
         STC   R1,PGDSNO                                                        
         LA    RE,PGDLEN(RE)                                                    
         ST    RE,LASTGSLT                                                      
         BCT   R9,GETHP2D                                                       
         BCTR  R1,0                                                             
         STC   R1,HPSNO                                                         
         B     REGNXT                                                           
         SPACE 2                                                                
GETHPX   CLI   VARFRMT,1           VARIABLE FORMAT                              
         BE    GETHPX2              YES - ALLOW VARIABLE SLOT                   
         L     R5,MEDAFRST         NO - FORCE TO DATE SLOT                      
         L     R1,PGNOENT                                                       
         LTR   R1,R1                                                            
         BZ    GETHPX2                                                          
         L     R8,VPGRID                                                        
         LA    R2,1                                                             
GETHPS   CLC   PGDWK,0(R5)                                                      
         BNL   *+12                                                             
         LA    R2,1                                                             
         L     R5,MEDAFRST                                                      
         CLC   PGDWK,2(R5)         SET FIXED SLOT FOR WEEK                      
         BNH   GETHPS2                                                          
         LA    R5,12(R5)                                                        
         LA    R2,1(R2)                                                         
         B     GETHPS                                                           
GETHPS2  STC   R2,PGLSLOT                                                       
         LA    R8,PGDLEN(R8)                                                    
         BCT   R1,GETHPS                                                        
GETHPX2  XIT1                                                                   
         DROP  R6                                                               
         DROP  R5                                                               
         DROP  R3                                                               
         DROP  R8                                                               
         LTORG                                                                  
         EJECT                                                                  
GETBUF   CSECT                                                                  
         NMOD1 0,GETBUF                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SUMDSECT,R3                                                      
         CLI   BUFHI,1                                                          
         BNE   GETBUF1                                                          
         XC    MYBUFIO(30),MYBUFIO                                              
         MVC   MYBUFIO(1),BUFCDE                                                
         MVI   BUFHI,0                                                          
         L     R8,BUFFBUFF                                                      
         SR    R9,R9                                                            
         IC    R9,LEVEL                                                         
         GOTO1 BUFFALO,DMCB,=C'HIGH',(BUFCDE,(R8)),MYBUFIO,(R9)                 
         B     GETBUF2                                                          
GETBUF1  L     R8,BUFFBUFF                                                      
         SR    R9,R9                                                            
         IC    R9,LEVEL                                                         
         GOTO1 BUFFALO,DMCB,=C'SEQ',(BUFCDE,(R8)),MYBUFIO,(R9)                  
GETBUF2  CLC   MYBUFIO(1),BUFCDE                                                
         BNE   *+12                                                             
         TM    DMCB+8,X'80'                                                     
         BZ    GETBUF3                                                          
         XC    MYBUFIO(30),MYBUFIO                                              
         B     GETBUFX                                                          
GETBUF3  MVC   PRTLINE,MYBUFIO                                                  
         LA    R3,MYBUFIO                                                       
         XC    MYBUFIO(8),MYBUFIO                                               
         MVI   SUMRPT,1                                                         
         MVC   SUMKEY,PRTLINE                                                   
         LA    R8,PRTLINE                                                       
         L     RF,BUFFBUFF                                                      
         USING BUFFALOD,RF                                                      
         A     R8,BUFFLKEY                                                      
         DROP  RF                                                               
GETBUFX  XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
* BUILD GRID DATES IN HEADLINES                                                 
HDATES   CSECT                                                                  
         NMOD1 0,HDATES                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         L     R7,MEDBUFF                                                       
         USING MEDBLOCK,R7                                                      
         USING SPWORKD,RA,RC                                                    
         USING SP60WK,R2                                                        
*                                                                               
HDATESX  XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
* PRINT POOL BUYSHEET GRID                                                      
*        0 - LENGTH OF GRID BLOCK                                               
*       1-3- A(SPWORK)                                                          
         SPACE 2                                                                
PTSGRID  CSECT                                                                  
         NMOD1 10,PTSGRID                                                       
         L     RA,0(R1)                                                         
         LA    RA,0(RA)                                                         
         LR    R3,RC                                                            
         USING PTSGD,R3                                                         
         MVC   GRIDLEN,0(R1)                                                    
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         USING SP60WK,R2                                                        
         L     R6,VPGRID                                                        
         USING PGRIDD,R6                                                        
         EJECT                                                                  
*        SORT SPOTS INTO DAY/BRAND/SPOT LENGTH/TYPE ORDER                       
         L     R6,VPGRID           SET UP SORT KEYS                             
         LR    RE,R6                                                            
         USING PGSRT1D,RE                                                       
         L     R1,PGNOENT                                                       
         LTR   R1,R1                                                            
         BZ    PTSGRIDX                                                         
         LA    R7,1                                                             
PGCNDSRT XC    PGSORT,PGSORT                                                    
         MVC   PGDS1WK,PGDWK                                                    
         MVI   PGDS1SLT,0                                                       
         CLI   PGCNDSW,1           CONDENSE REQUIRED                            
         BE    *+8                  YES - DONT SET SPOT NUMDBER                 
         STC   R7,PGDS1SLT          NO - SET SPOT NUMBER                        
         MVC   PGDSNO,PGDS1SLT                                                  
         MVC   PGDS1BR,PGDSBRN                                                  
         MVC   PGDS1SL,PGDSSLN                                                  
         MVC   PGDS1IND,PGDIND                                                  
         MVC   PGDS1CUT,PGDCUTIN                                                
         MVC   PGDS1ADY,PGDAFDAY                                                
         MVC   PGDS1ATM,PGDAFTIM                                                
         CLI   PGDIND,0            OTO                                          
         BNE   *+8                                                              
         LA    R7,1(R7)             NO - BUMP SPOT NUMBER                       
         LA    RE,PGDLEN(RE)                                                    
         LR    R6,RE                                                            
         BCT   R1,PGCNDSRT                                                      
         DROP  RE                                                               
         L     R8,PGNOENT                                                       
         L     R6,VPGRID                                                        
         GOTO1 XSORT,DMCB,(R6),(R8),PGDLEN,PGSRTLN,0                            
         SPACE 2                                                                
*        CONDENSE LIKE SPOTS                                                    
         CLI   PGCNDSW,1           CONDENSE - REQUIRED                          
         BNE   PGCNDX               NO - CHECK FORMAT TYPE                      
*              TABLE SORTED NOW CONDENSE                                        
         L     R6,VPGRID                                                        
PGCND1   OC    0(PGDLEN,R6),0(R6)  END                                          
         BZ    PGCND2                                                           
         MVI   PGDSNO,1            SET SPOT NUMBER TO 1                         
         LA    R6,PGDLEN(R6)                                                    
         B     PGCND1                                                           
         SPACE 2                                                                
PGCND2   L     R6,VPGRID                                                        
         LR    RE,R6                                                            
         LR    R4,R6                                                            
PGCND3   LA    R4,PGDLEN(R4)       SET POINTER TO NEXT SLOT                     
         OC    0(PGDLN1,R4),0(R4)                                               
         BZ    PGCND4                                                           
         CLC   0(L'PGSORT,RE),0(R4)     THIS SLOT TO NEXT                       
         BNE   PGCND3A                                                          
         LR    R6,RE                    POINT TO ADD SLOT                       
         ZIC   RF,PGDSNO           BUMP NUMBER OF SPOTS BY 1                    
         LA    RF,1(RF)                                                         
         STC   RF,PGDSNO                                                        
         LR    R6,R4                                                            
         MVI   PGSORT,X'FF'             ELIMINATE NEXT ELEMENT                  
         B     PGCND3                                                           
PGCND3A  LR    RE,R4                                                            
         B     PGCND3                                                           
PGCND4   L     R6,VPGRID                                                        
         L     R8,PGNOENT                                                       
         GOTO1 XSORT,DMCB,(R6),(R8),PGDLEN,PGSRTLN,0                            
         L     R6,VPGRID                DELETE DUPS                             
         L     R1,PGNOENT                                                       
PGCND5   CLI   0(R6),0                                                          
         BE    PGCNDX                                                           
         CLI   0(R6),X'FF'                                                      
         BNE   PGCND6                                                           
         XC    0(PGDLEN,R6),0(R6)                                               
         BCTR  R1,0                                                             
         ST    R1,PGNOENT                                                       
PGCND6   LA    R6,PGDLEN(R6)                                                    
         B     PGCND5                                                           
         SPACE 2                                                                
PGCNDX   DS    0H                                                               
         EJECT                                                                  
*              CHECK FOR VARIABLE FORMAT                                        
         CLI   VARFRMT,1                                                        
         BNE   VAR2                                                             
         LA    R5,1                INITIALIZE SLOT NUMBER                       
         L     R6,VPGRID                                                        
         SPACE 2                                                                
VARF1    OC    PGSORT,PGSORT                                                    
         BZ    VARFX                                                            
         STC   R5,PGLSLOT          SET SLOT NUMBER                              
         CLC   PGDLEN(10,R6),PGSORT                                             
         BNE   VARF2                                                            
         LA    R6,PGDLEN(R6)                                                    
         B     VARF1                                                            
VARF2    LA    R6,PGDLEN(R6)                                                    
         LA    R5,1(R5)                                                         
         B     VARF1                                                            
         SPACE 2                                                                
VAR2     CLI   VARFRMT,2           VARIABLE FORMAT WITH FIXED SLOTS             
         BNE   VARFX                                                            
         LA    R5,1                INITIALIZE SLOT NUMBER                       
         L     R6,VPGRID                                                        
         SPACE 2                                                                
VAR2F1   OC    PGSORT,PGSORT                                                    
         BZ    VARFX                                                            
         STC   R5,PGLSLOT          SET SLOT NUMBER                              
         CLC   PGDLEN(2,R6),PGSORT SLOT BY DATE ONLY                            
         BNE   VAR2F2                                                           
         LA    R6,PGDLEN(R6)                                                    
         B     VAR2F1                                                           
VAR2F2   LA    R6,PGDLEN(R6)                                                    
         LA    R5,1(R5)                                                         
         B     VAR2F1                                                           
         SPACE 2                                                                
VARFX    DS    0H                                                               
         EJECT                                                                  
*              SORT INTO SLOT NUMBER ORDER                                      
         L     R6,VPGRID                                                        
         LR    RE,R6                                                            
         USING PGSRT2D,RE                                                       
         L     R1,PGNOENT                                                       
PGSLTSRT XC    PGSORT,PGSORT                                                    
         LR    RE,R6                                                            
         MVC   PGDS2SLT,PGLSLOT                                                 
         MVC   PGDS2WK,PGDWK                                                    
         MVI   PGDS2SNO,0                                                       
         CLI   PGCNDSW,1                                                        
         BE    *+10                                                             
         MVC   PGDS2SNO,PGDSNO                                                  
         MVC   PGDS2BR,PGDSBRN                                                  
         MVC   PGDS2SL,PGDSSLN                                                  
         MVC   PGDS2IND,PGDIND                                                  
         LA    R6,PGDLEN(R6)                                                    
         BCT   R1,PGSLTSRT                                                      
         DROP  RE                                                               
         L     R8,PGNOENT                                                       
         L     R6,VPGRID                                                        
         GOTO1 XSORT,DMCB,(R6),(R8),PGDLEN,PGSRTLN,0                            
         EJECT                                                                  
*              TABLE SORTED INTO SLOT NUMBER ORDER                              
*               NOW ASSIGN LINE NUMBERS                                         
PGSLNO   L     R6,VPGRID                                                        
         LA    R5,1                                                             
         LH    RF,NOINGRID                                                      
         ST    RF,PGWMAX                                                        
PGSLNO1  MVC   WORK(L'PGSORT),PGSORT                                            
         STC   R5,PGSUBLI          SET SUB-LINE                                 
         ZIC   RF,PGLSLOT                                                       
         BCTR  RF,0                                                             
         SR    RE,RE                                                            
         D     RE,PGWMAX           GET LINE NUMBER                              
         STC   RF,PGLINNO                                                       
         STC   RE,PGLSLOT                                                       
         LA    R6,PGDLEN(R6)                                                    
         CLC   PGSORT(1),WORK                                                   
         BNE   PGSLNO2                                                          
         LA    R5,1(R5)                                                         
         B     PGSLNO1                                                          
         SPACE 2                                                                
PGSLNO2  LA    R5,1                                                             
         OC    PGSORT,PGSORT                                                    
         BNZ   PGSLNO1                                                          
         SPACE 2                                                                
*              SORT INTO LINE/SUBLINE/SLOT NUMBER ORDER                         
         L     R6,VPGRID                                                        
         L     R1,PGNOENT                                                       
PGSRTPR  XC    PGSORT,PGSORT                                                    
         MVC   PGSORT(3),PGLINNO                                                
         LA    R6,PGDLEN(R6)                                                    
         BCT   R1,PGSRTPR                                                       
         SPACE 2                                                                
         L     R8,PGNOENT                                                       
         L     R6,VPGRID                                                        
         GOTO1 XSORT,DMCB,(R6),(R8),PGDLEN,PGSRTLN,0                            
         EJECT                                                                  
         L     RE,ADBUY                                                         
         USING BUYREC,RE                                                        
         MVC   PGPRLNO,PGLINNO                                                  
         XC    PRPGWMAX,PRPGWMAX                                                
         MVC   GRIDSLN,BDSEC                                                    
PTSGRDA  MVC   GRIDST,DSTAGRID                                                  
         XC    PGWKCNT,PGWKCNT                                                  
         XC    PGWMAX,PGWMAX                                                    
         BAS   R9,SETAST                                                        
         SPACE 2                                                                
PTSGRID1 L     R4,GRIDST           SET BEGINNING OF BLOCK                       
         ZIC   RF,PGLSLOT                                                       
         ST    RF,PGWKCNT                                                       
         ZIC   RF,GRIDLEN                                                       
         MH    RF,PGWKCNT+2         SET BEGINNING OF THIS SLOT                  
         AR    R4,RF                                                            
PTSGRID2 OC    PGDWK,PGDWK         END                                          
         BZ    PTSGRIDX                                                         
         CLC   PGLINNO(2),PGPRLNO  SAME LINE/SUBLINE                            
         BE    PTSGRID3                                                         
         CLI   FORCEHED,C'Y'                                                    
         BE    PTSGRD2                                                          
         MVC   PGPRLNO,PGLINNO                                                  
         L     RE,PGWMAX            YES - SET NEXT GRID                         
         XC    PGWMAX,PGWMAX                                                    
         A     RE,PRPGWMAX                                                      
         ST    RE,PRPGWMAX                                                      
         C     RE,=F'7'                                                         
         BNH   PTSGRD2A                                                         
PTSGRD2  GOTO1 REPORT                                                           
         XC    PRPGWMAX,PRPGWMAX                                                
         B     PTSGRDA                                                          
PTSGRD2A MH    RE,=H'132'                                                       
         A     RE,DSTAGRID                                                      
         ST    RE,GRIDST                                                        
         BAS   R9,SETAST                                                        
         B     PTSGRID1                                                         
         EJECT                                                                  
* SET ALLOCATIONS IN PRINT LINE                                                 
PTSGRID3 ICM   R5,15,PGDELAD                                                    
         USING REGELEM,R5                                                       
I13GRID  DS    0C                                                               
         MVI   PGWNOL,0                                                         
         BAS   R9,LINEA                                                         
         ST    RE,FULL                                                          
         XC    DUB,DUB                                                          
         ZIC   RE,MAXLINES                                                      
         ZIC   RF,LINE                                                          
         SR    RE,RF                                                            
         CH    RE,=H'4'                                                         
         BH    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
* GET DATES                                                                     
         L     R9,FULL                                                          
         GOTO1 DATCON,DMCB,(2,RDATE),(4,(R9))                                   
         GOTO1 DATCON,DMCB,(2,RDATE),DUB                                        
*--->    GOTO1 GETDAY,DMCB,DUB,WORK                                             
*--->    NI    DMCB,X'0F'                                                       
         MVC   WORK(6),DUB                                                      
*--->    ZIC   RE,DMCB                                                          
         ZIC   R1,PGDFDAY                                                       
         SR    R0,R0                                                            
         SLL   R1,25                                                            
         SR    R9,R9                                                            
I13GRD2A LTR   R1,R1                                                            
         BZ    I13GRD2B                                                         
         SLDL  R0,1                                                             
         LTR   R0,R0               DON'T COUNT THE FIRST                        
         BZ    *+8                                                              
         LA    R9,1(R9)                                                         
         B     I13GRD2A                                                         
I13GRD2B DS    0C                                                               
*--->    SR    R9,RE                                                            
         BCTR  R9,0                                                             
         LTR   R9,R9                                                            
         BZ    I13GRD2C                                                         
         GOTO1 ADDAY,DMCB,WORK,DUB,(R9)                                         
         L     R9,FULL                                                          
         LA    R9,6(R9)                                                         
         GOTO1 DATCON,DMCB,(0,DUB),(4,(R9))                                     
         L     RE,FULL                                                          
         MVI   5(RE),C'-'                                                       
I13GRD2C TM    PGDIND,X'04'                                                     
         BZ    I13GRD2D                                                         
         L     RE,FULL                                                          
         MVC   12(7,RE),=C'PREEMPT'                                             
I13GRD2D DS    0C                                                               
         TM    PGDIND,X'01'                                                     
         BZ    I13GRD2E                                                         
         L     RE,FULL                                                          
         MVC   12(7,RE),=C'MISSED'                                              
I13GRD2E DS    0C                                                               
         TM    PGDIND,X'08'                                                     
         BZ    I13GRD2F                                                         
         L     RE,FULL                                                          
         MVC   12(7,RE),=C'*MINUS*'                                             
I13GRD2F DS    0C                                                               
         ZIC   RE,PGWNOL                                                        
         LA    RE,1(RE)                                                         
         STC   RE,PGWNOL                                                        
I13GRID3 BAS   R9,LINEA                                                         
         ZIC   RF,PGDSBRN                                                       
         BCTR  RF,0                                                             
         MH    RF,PRDBUFLN                                                      
         A     RF,PRDBUFF                                                       
         MVC   0(3,RE),1(RF)                                                    
         CLI   PGD2BRN,0                                                        
         BE    I13GRD3A                                                         
         ZIC   RF,PGD2BRN                                                       
         BCTR  RF,0                                                             
         MH    RF,PRDBUFLN                                                      
         A     RF,PRDBUFF                                                       
         MVI   3(RE),C'/'                                                       
         MVC   4(3,RE),1(RF)                                                    
I13GRD3A DS    0C                                                               
         ZIC   RE,PGWNOL                                                        
         LA    RE,1(RE)                                                         
         STC   RE,PGWNOL                                                        
         BAS   R9,LINEA                                                         
         LR    R9,RE                                                            
         EDIT  PGDCUTIN,(14,(R9)),2,COMMAS=YES,ALIGN=LEFT                       
         OC    RPAY,RPAY                                                        
         BZ    *+10                                                             
         MVC   15(4,R9),=C'CLRD'                                                
         ZIC   RE,PGWNOL                                                        
         LA    RE,1(RE)                                                         
         STC   RE,PGWNOL                                                        
         CLI   PGCNDSW,1                                                        
         BNE   I13GRD3B                                                         
         CLI   PGDSNO,1                                                         
         BNH   I13GRD3B                                                         
         BAS   R9,LINEA                                                         
         LR    R9,RE                                                            
         EDIT  PGDSNO,(2,(R9))                                                  
         MVC   3(5,R9),=C'SPOTS'                                                
         ZIC   RE,PGWNOL                                                        
         LA    RE,1(RE)                                                         
         STC   RE,PGWNOL                                                        
I13GRD3B DS    0H'0'                                                            
         XC    WORK,WORK                                                        
         BAS   R9,LINEA                                                         
         OC    PGDAFDAY,PGDAFDAY                                                
         BZ    I14GRIDX                                                         
         GOTO1 DATCON,DMCB,(2,PGDAFDAY),DUB                                     
         GOTO1 GETDAY,DMCB,DUB,WORK                                             
         XC    FULL,FULL                                                        
         GOTO1 DATCON,DMCB,(2,PGDAFDAY),(4,WORK+3)                              
         MVC   FULL(2),PGDAFTIM                                                 
         MVI   WORK+8,C','                                                      
         MVC   WORK+9(10),SPACES                                                
         GOTO1 UNTIME,DMCB,FULL,WORK+9                                          
         MVI   WORK+2,C','                                                      
         BAS   R9,LINEA                                                         
I14GRIDX MVC   0(14,RE),WORK                                                    
         ZIC   RE,PGWNOL                                                        
         LA    RE,1(RE)                                                         
         STC   RE,PGWNOL                                                        
         LR    R1,R4                                                            
         L     R4,GRIDST                                                        
         BAS   R9,LINEA                                                         
         MVC   0(2,RE),=C'- '                                                   
         MVC   2(130,RE),0(RE)                                                  
         LR    R4,R1                                                            
         ZIC   RE,PGWNOL                                                        
         LA    RE,1(RE)                                                         
         STC   RE,PGWNOL                                                        
         C     RE,PGWMAX                                                        
         BNH   *+8                                                              
         ST    RE,PGWMAX                                                        
         LA    R6,PGDLEN(R6)                                                    
         B     PTSGRID1                                                         
PTSGRIDX GOTO1 REPORT                                                           
         XMOD1 1                                                                
         EJECT                                                                  
LINEA    LR    RE,R4                                                            
         ZIC   RF,PGWNOL                                                        
         MH    RF,=H'132'                                                       
         AR    RE,RF                                                            
         LA    RE,1(RE)                                                         
         BR    R9                                                               
SETAST   L     RE,GRIDST                                                        
         XC    0(26,RE),0(RE)                                                   
         MVI   0(RE),C'*'                                                       
         MVC   26(110,RE),0(RE)                                                 
         LR    RF,RE                                                            
         LA    R1,3                                                             
         LA    RF,132(RF)                                                       
         MVC   0(132,RF),0(RE)                                                  
         BCT   R1,*-10                                                          
         BR    R9                                                               
         LTORG                                                                  
         BR    R9                                                               
         EJECT                                                                  
SORTC    CSECT                                                                  
         NMOD1 0,SORTC                                                          
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         USING SP60WK,R2                                                        
         CLI   SORTPASS,1          INPUT PHASE                                  
         BNE   SORTOUT              NO - DO OUTPUT                              
         OC    SSCNTR,SSCNTR                                                    
         BNZ   SORTIN1                                                          
         L     RE,VSSTABLE                                                      
         L     RF,=F'7100'                                                      
         XCEF                                                                   
         L     RE,VPNTABLE                                                      
         L     RF,=F'2600'                                                      
         XCEF                                                                   
SORTIN1  DS    0H                                                               
         XC    WORK,WORK                                                        
         LA    RE,WORK                                                          
         USING PNAMD,RE                                                         
         L     R5,ADBUY                                                         
         USING BUYREC,R5                                                        
         MVC   WORK(1),PDNCNTR+3                                                
         MVC   PNDNAME,BDPROG+4                                                 
         MVC   PNDNAME+15(1),BDSEC                                              
         L     R9,PDNCNTR                                                       
         L     R8,VPNTABLE                                                      
         GOTO1 BINSRCH,DMCB,(1,WORK),(R8),(R9),18,(1,17),255                    
         OC    DMCB,DMCB                                                        
         BNZ   *+6                                                              
         DC    H'0'                PROGRAM NAME TABLE IS FULL                   
         MVC   PDNCNTR,DMCB+8                                                   
         L     R1,DMCB                                                          
         MVC   CURRPNUM,0(R1)                                                   
         XC    WORK,WORK                                                        
         DROP  RE                                                               
         LA    RE,WORK                                                          
         USING SQSTART,RE                                                       
         MVC   SORTKLEN,=F'4'                                                   
         MVC   SORTRLEN,=F'8'                                                   
         MVC   DADRDISP,=F'4'                                                   
         CLI   SORTFRMT,1                                                       
         BNE   SORTIN2                                                          
         MVC   SQ1DAY,BDDAY                                                     
         XI    SQ1DAY,X'FF'                                                     
         MVC   SQ1TIME,BDTIMST                                                  
         MVC   SQ1PNUM,CURRPNUM                                                 
         MVC   SQ1DADDR,KEY+14                                                  
         B     SORTADD                                                          
         SPACE 2                                                                
SORTIN2  CLI   SORTFRMT,2                                                       
         BNE   SORTIN3                                                          
         MVC   SQ2DAY,BDDAY                                                     
         XI    SQ2DAY,X'FF'                                                     
         MVC   SQ2TIME,BDTIMST                                                  
         MVC   SQ2PNUM,CURRPNUM                                                 
         MVC   SQ2DADDR,KEY+14                                                  
         B     SORTADD                                                          
SORTIN3  B     SORTCX                                                           
         SPACE 2                                                                
* ADD A RECORD TO THE SORT BUFFER                                               
SORTADD  L     RF,SSCNTR                                                        
         SR    RE,RE                                                            
         M     RE,SORTRLEN                                                      
         A     RF,VSSTABLE                                                      
         MVC   0(20,RF),WORK                                                    
         L     RF,SSCNTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,SSCNTR                                                        
         MVI   SOUTFRST,1                                                       
         B     SORTCX                                                           
         EJECT                                                                  
SORTOUT  CLI   SOUTFRST,1                                                       
         BNE   SRTOUT1                                                          
         L     R4,VSSTABLE                                                      
         L     R5,SSCNTR                                                        
         L     R6,SORTRLEN                                                      
         L     R7,SORTKLEN                                                      
         GOTO1 XSORT,DMCB,(R4),(R5),(R6),(R7),0                                 
         MVI   SOUTFRST,0                                                       
         MVC   NEXTSSLT,VSSTABLE                                                
         L     RE,ADBUY                                                         
         ST    RE,AREC                                                          
         SPACE 2                                                                
SRTOUT1  L     RE,NEXTSSLT                                                      
         A     RE,DADRDISP                                                      
         MVC   KEY+14(4),0(RE)                                                  
         OC    KEY+14(4),KEY+14                                                 
         BNZ   SRTOUT2                                                          
         MVI   SORTPASS,3                                                       
         MVI   SOUTFRST,1                                                       
         XC    SSCNTR,SSCNTR                                                    
         B     SORTCX                                                           
SRTOUT2  L     RE,ADBUY            GET A BUY RECORD                             
         ST    RE,AREC                                                          
         GOTO1 GET                                                              
         L     RE,ADBUY                                                         
         MVC   KEY(13),0(RE)                                                    
         L     RE,NEXTSSLT                                                      
         XC    CURRSORT,CURRSORT   SET CURRENT KEY AND NEXT KEY                 
         XC    NEXTSORT,NEXTSORT                                                
         L     R9,SORTKLEN                                                      
         BCTR  R9,0                                                             
         EX    R9,*+8                                                           
         B     *+10                                                             
         MVC   CURRSORT(0),0(RE)                                                
         A     RE,SORTRLEN                                                      
         ST    RE,NEXTSSLT                                                      
         EX    R9,*+8                                                           
         B     *+10                                                             
         MVC   NEXTSORT(0),0(RE)                                                
         MVI   SORTPASS,2                                                       
SORTCX   XMOD1 1                                                                
         LTORG                                                                  
SORTKLEN DC    F'0'                SORT KEY LENGTH                              
SORTRLEN DC    F'0'                SORT RECORD LENGTH                           
DADRDISP DC    F'0'                DISK ADDRESS DISPLACEMENT                    
NEXTSSLT DC    F'0'                NEXT SORT SLOT                               
SOUTFRST DC    X'00'               FIRST TIME SWITCH                            
         EJECT                                                                  
         EJECT                                                                  
PTSGD    DSECT                                                                  
GRIDST   DS    F                   START OF GRID                                
PRPGWMAX DS    F                                                                
PRPGDWK  DS    CL2                 PREVIOUS BLOCK DATES                         
PGPRLNO  DS    CL3                 PREVIOUS LINE/SUN LINE/SLOT                  
PGWNOL   DS    C                   NUMBER OF LINES IN THIS BLOCK                
GRIDLEN  DS    C                                                                
GRIDSW1  DS    C                                                                
GRIDSLN  DS    C                                                                
PGWKCNT  DS    F                   WEEKLY SLOT COUNTER                          
         EJECT                                                                  
         EJECT                                                                  
SP60WK   CSECT                                                                  
RELO     DS    F                                                                
RTYPE    DS    CL3                                                              
BUFHI    DS    C                                                                
BUFCDE   DS    C                                                                
BUFRTYP  DS    C                                                                
LEVEL    DS    C                   LEVEL CODE                                   
ESTACT   DS    CL1                                                              
CPPSW    DC    C'D'                                                             
LVCNTRL  DC    F'1'                                                             
         DC    A(2,3,4,5)                                                       
DNAMES   DS    0CL28                                                            
DNAME1   DS    CL7                                                              
DNAME2   DS    CL7                                                              
DNAME3   DS    CL7                                                              
DNAME4   DS    CL7                                                              
SUBPROG  DS    0CL8                                                             
         DC    C'SP'                                                            
SUBPROG1 DC    C'M2'                                                            
SUBPROG2 DC    C'01'                                                            
         DC    C'  '                                                            
DASH     DC    80C'-'                                                           
MYBUFIO  DS    CL200                                                            
FIRST    DS    C                                                                
PASS     DS    C                                                                
MAXPASS  DS    C                   HIGHEST REQUIRED PASS                        
SCNDDTSW DC    X'01'               PRINT DATE ON SECOND LINE                    
REQPRD2  DS    C                   QPRD2 PRODUCT NUMBER                         
PRTLINE  DS    CL132                                                            
VCALCPP  DC    F'0'                                                             
VSUMMRY  DC    F'0'                                                             
REPCALOV DC    F'0'                                                             
APTSDESC DC    F'0'                                                             
APRSDESC DC    F'0'                                                             
PSTASPT  DC    F'0'                                                             
PSTACOST DC    F'0'                                                             
PSTAGRID DC    F'0'                                                             
DSTAGRID DC    F'0'                A(DETAIL STATION GRID PRINT)                 
DDESC    DC    F'0'                A(DETAIL DESCRIPTION PRINT)                  
DTOTSPT  DC    F'0'                A(DETAIL TOTAL SPOTS)                        
PENNYSW  DS    C                                                                
SPACESW  DS    CL1                                                              
PIGBRND  DS    C                                                                
HLDBDTIM DS    C                                                                
HLDPNAM  DS    CL14                                                             
HPSNO    DS    C                                                                
LASTGSLT DS    F                                                                
HLDBOOK  DS    CL2                                                              
SVDESCL  DS    CL132                                                            
         DS    0F                                                               
SALSDATA DS    0CL28               SALESPERSONS WORK AREA                       
SALSWKS  DS    F                                                                
SALSSPT  DS    F                                                                
SALSD1   DS    F                                                                
SALSD2   DS    F                                                                
SALSD3   DS    F                                                                
SALSD4   DS    F                                                                
SALSDL   DS    F                                                                
PRTADDR  DS    F                                                                
SVPH01   DS    F                                                                
SVPH02   DS    F                                                                
SVPH04   DS    F                                                                
SVSPECS  DS    F                                                                
SVMDTAB  DS    F                                                                
SVRDTE   DS    F                                                                
VMDADDWT DC    F'0'                                                             
VSTATOT  DC    F'0'                                                             
VEDTDEMS DC    F'0'                                                             
VHDATES  DC    F'0'                                                             
VGETBUF  DC    F'0'                                                             
VFLMPRD  DS    F                                                                
VSUBPARA DC    F'0'                                                             
VPGRID   DS    F                                                                
VCOMPRNT DS    F                                                                
PGHILNO  DS    F                   HIGHEST LINE NUMBER                          
PGNOENT  DS    F                                                                
PGCNDSW  DS    C                   CONDENSE LIKE SPOTS                          
PGCURLNO DS    F                   CURRENT LINE #                               
PGWMAX   DS    F                   MAXIMUM SLOTS                                
VARFRMT  DS    C                   VARIABLE FORMAT                              
AHDATES  DC    F'0'                                                             
SVRCSUB  DS    C                                                                
MSRCSUB  DS    C                                                                
SVSUPMKT DS    C                                                                
MSSUPMKT DS    C                                                                
DPTSW    DS    C                   DAYPART CHANGE SWITCH                        
MRPTTYP  DS    C                                                                
BUYACT   DS    C                                                                
SPOTACT  DS    C                                                                
LINEACT  DS    C                                                                
CFDS     DS    C                                                                
CFDE     DS    C                                                                
OVRFLAG  DS    CL16                                                             
STRDTE   DS    CL2                                                              
ENDDTE   DS    CL2                                                              
SVMGC1   DS    F                                                                
SVMGC2   DS    F                                                                
SVPGC1   DS    F                                                                
SVPGC2   DS    F                                                                
MSHDHOOK DC    F'0'                                                             
SVHDHOOK DC    F'0'                                                             
MSSPHK   DC    F'0'                                                             
SVSPHK   DC    F'0'                                                             
VGETREP  DC    F'0'                                                             
MYSPTHOK DC    F'0'                                                             
VFOOT    DC    F'0'                                                             
*                                                                               
* DETAIL OPTIONS - CONTROLLED BY QOPT2                                          
*                     1=YES,0=NO                                                
*                                                                               
*               FIELD  OPTION                                                   
*               -----  ------                                                   
*                 1    PRINT '*' NEXT TO OVERRIDEN DEMOS                        
*                 2    PRINT DEMO VALUES                                        
*                 3    PRINT COST                                               
*                 4    PRINT CPP                                                
DETOPTS  DS    CL4                 CURRENT OPTIONS SAVE                         
DETOPT   DC    AL1(1,1,1,1)        OPTION TABLE                                 
         DC    AL1(0,1,1,1)                                                     
         DC    AL1(0,0,1,1)                                                     
         DC    AL1(0,1,0,0)                                                     
         DC    AL1(1,1,0,0)                                                     
         DC    AL1(0,0,0,0)                                                     
*                                                                               
* TOTAL OPTIONS - CONTROLLED BY QOPT5                                           
*                        1=YES,0=NO                                             
*                                                                               
*               FIELD  OPTION                                                   
*               -----  ------                                                   
*                 1    PRINT TELECASTS                                          
*                 2    PRINT DOLLARS                                            
*                 3    PRINT DEMOS                                              
*                                                                               
SUMOPTS  DS    CL3                                                              
SUMOPT   DC    AL1(1,1,1)                                                       
         DC    AL1(1,1,0)                                                       
         DC    AL1(1,0,0)                                                       
         DC    AL1(1,0,1)                                                       
         DC    AL1(0,0,1)                                                       
         DC    AL1(0,1,0)                                                       
         DC    AL1(0,1,1)                                                       
         DC    AL1(0,0,0)                                                       
*                                                                               
* DATE OPTIONS - CONTROLLED BY PROFDCTL                                         
*                        SETS VARFRMT AND SCNDDTSW                              
DATEOPTS DC    X'00',AL1(0,0)                                                   
         DC    C' ',AL1(0,0)                                                    
         DC    C'0',AL1(0,0)                                                    
         DC    C'1',AL1(0,1)                                                    
         DC    C'2',AL1(1,0)                                                    
         DC    C'3',AL1(1,1)                                                    
         DC    C'4',AL1(2,0)                                                    
         DC    C'5',AL1(2,1)                                                    
         DC    X'FF',AL1(0,0)                                                   
DATEOPT  DS    CL2                                                              
* SORT OPTIONS      0 = NO SORT                                                 
*                   1 = DAY/TIME/PROGRAM                                        
*                   2 = TIME/DAY/PROGRAM                                        
SORTOPTS DC    C' ',AL1(0)                                                      
         DC    C'1',AL1(1)                                                      
         DC    C'2',AL1(2)                                                      
         DC    X'FF',AL1(0)                                                     
SORTOPT  DS    CL1                                                              
*                                                                               
* REPORT FORMAT OPTIONS                                                         
*                   FIELD1 = LENGTH OF GRID                                     
*                   FIELD2 = NUMBER IN GRID FOR PTS                             
*                   FIELD3 = NUMBER IN GRID FOR RS                              
*                                                                               
FRMTOPTS DC    X'00',AL1(26,5,5)                                                
         DC    X'FF',AL1(22,5,5)                                                
FRMTOPT  DS    CL3                                                              
* CONDENSE OPTIONS                                                              
*                        SETS PGCNDSW                                           
*                                                                               
CNDSOPTS DC    X'00',AL1(0)                                                     
         DC    C'1',AL1(1)                                                      
         DC    C'2',AL1(2)                                                      
         DC    X'FF',AL1(0)                                                     
CNDSOPT  DS    CL1                                                              
SVDEMS   DS    0F                                                               
SVD1     DS    F                   DEMO 1 VALUE                                 
SVD1CP   DS    F                   DEMO 1 CPP/CPM                               
SVD2     DS    F                   DEMO 2 VALUE                                 
SVD2CP   DS    F                   DEMO 2 CPP/CPM                               
SVD3     DS    F                   DEMO 3 VALUE                                 
SVD3CP   DS    F                   DEMO 3 CPP/CPM                               
SVD4     DS    F                   DEMO 4 VALUE                                 
SVD4CP   DS    F                   DEMO 4 CPP/CPM                               
SVDG     DS    F                                                                
SVDGCP   DS    F                                                                
ACTMO    DS    F                                                                
UNIVERSE DS    F                                                                
WEIGHT   DS    F                                                                
MCOUNT   DS    F                                                                
PLDEMS   DS    0CL44                                                            
PLD1     DS    CL5                                                              
PLD1CP   DS    CL6                                                              
PLD2     DS    CL5                                                              
PLD2CP   DS    CL6                                                              
PLD3     DS    CL5                                                              
PLD3CP   DS    CL6                                                              
PLD4     DS    CL5                                                              
PLD4CP   DS    CL6                                                              
STAGRID  DS    14F                                                              
STASPOT  DS    F                   STATION TOTAL SPOTS                          
STADEMS  DS    8F                  STATION TOTAL DEMOS                          
STACOST  DS    2F                  STATION TOTAL DOLLARS                        
STACINV  DS    F                                                                
STACOSTN DS    F                                                                
STTSPOT  DS    F                   OVERALL STATION COST                         
STTDEMS  DS    8F                  OVERALL STATION DEMOS                        
STTCOST  DS    2F                  OVERALL STATION DOLLARS                      
VSVMDBLK DS    F                                                                
STAT1    DS    C                                                                
STAT2    DS    C                                                                
FILMNO   DS    CL8                                                              
STACAP   DS    CL7                                                              
MSOPT    DS    CL7                                                              
SVOPTS   DS    CL7                                                              
SUBPSW   DS    CL1                                                              
MSPROF   DS    CL16                                                             
SVPROF   DS    CL16                                                             
MSBFHOOK DS    F                                                                
PASSSD3  DS    CL3                                                              
PASSED3  DS    CL3                                                              
PKGAREA  DS    CL16                BUY TYPE CAPTIONS                            
PIGAREA  DS    CL32                PIGGYBACK AREA                               
PIGAREAL DS    CL3                 PIGGYBACK LENGTH                             
PIGPRNT  DS    CL11                PIGGYBACK PRINT                              
         DS    0F                                                               
PREMTAB  DS    CL64                                                             
HIATAB   DS    CL64                                                             
COMAREA  DS    CL400               COMMENT AREA                                 
COVRHLD  DS    396C                COST OVERRIDE HOLD AREA                      
COVRFRST DS    C                   COST OVR FIRST TIME                          
OVRCNT   DS    C                   PL OVERRIDE COUNT                            
SVMAXLIN DS    C                                                                
MGSW     DS    C                                                                
HAVPROD  DS    C                                                                
APL      DS    F'0'                A(PL) FOR COST OVERRIDES                     
SVQEND   DS    CL6                                                              
SVSPREP  DS    H                                                                
SVP1     DS    CL40                                                             
SVP2     DS    CL40                                                             
SVP3     DS    CL40                                                             
SVP4     DS    CL40                                                             
PAYREP   DS    CL20                                                             
CONREP   DS    CL20                                                             
PANAME   DS    CL20                                                             
PA1      DS    CL24                                                             
PA2      DS    CL24                                                             
PA3      DS    CL8                                                              
NUMWK    DS    F                                                                
NOINGRID DS    H                                                                
LENGRID  DS    C                                                                
CURRSORT DS    CL20                                                             
NEXTSORT DS    CL20                                                             
OPTRMODE DS    C                                                                
OPTRPT   DS    F                                                                
VPNTABLE DC    F'0'                                                             
PDNCNTR  DC    F'0'                                                             
VSSTABLE DC    F'0'                                                             
SSCNTR   DC    F'0'                                                             
VRSORT   DC    F'0'                                                             
ACISLIST DC    F'0'                                                             
VSTACUT  DC    F'0'                                                             
ASTACUT  DC    F'0'                                                             
CURRCIS  DC    F'0'                                                             
CSPNO    DC    F'0'                                                             
SORTPASS DS    C                                                                
SORTREQ  DS    C                                                                
SORTFRMT DS    C                                                                
CURRPNUM DS    C                                                                
PASSTAB  DS    CL144               LIST OF PASS START-END DATES                 
PASSQST  DS    CL12                THIS PASS START-END                          
REASTART DS    CL12                REQUEST START-END DATES                      
SVRCPROG DS    CL2                                                              
IQREP    DS    CL3                                                              
IQMGR    DS    C                                                                
IQPGR    DS    C                                                                
IQINV    DS    CL10                                                             
CURRREP  DS    CL2                                                              
REPLIST  DS    CL200                                                            
PASSTABD DSECT                     SAVE MEDBLOCKS FOR PASSES                    
PASSSD   DS    CL6                 START DATE                                   
PASSED   DS    CL6                 END DATE                                     
PASSP1   DS    CL28                MEDBLOCK LEADER                              
PASSP2   DS    CL168               WEEKS                                        
PASSP3   DS    CL48                MONTHS                                       
PASSP4   DS    CL12                PERIOD                                       
PASSEND  DS    0C                                                               
PNTABLE  CSECT                                                                  
         DS    2600C                                                            
SSTABLE  CSECT                                                                  
         DS    7100C                                                            
PGRIDC   CSECT                                                                  
         DS    15000C                                                           
SVMDBLK  CSECT                                                                  
         DS    1208C               SAVE MEDIA SUMMARY MEDBLOCK                  
STACUT   CSECT                                                                  
         DS    15000C                                                           
         LTORG                                                                  
SUBPAREA CSECT                                                                  
         DS    90000C                                                           
         EJECT                                                                  
BPRTD    DSECT                                                                  
BPRTSPT  DS    CL10                 0                                           
BPRTD1   DS    CL7                 11                                           
BPRTDL   DS    CL9                 18                                           
BPRTD1C  DS    CL8                 27                                           
BPRTD2   DS    CL7                 35                                           
BPRTD2C  DS    CL8                 43                                           
BPRTD3   DS    CL7                 51                                           
BPRTD3C  DS    CL8                 58                                           
BPRTD4   DS    CL7                 68                                           
BPRTD4C  DS    CL8                 76                                           
         EJECT                                                                  
PGRIDD   DSECT                                                                  
PGSORT   DS    CL14                                                             
PGLINNO  DS    C                   PRINT BLOCK LINE NUMBER                      
PGSUBLI  DS    C                   SUB-LINE NUMBER                              
PGLSLOT  DS    C                   PRINT BLOCK SLOT NUMBER                      
PGDWK    DS    CL2                 WEEK OF                                      
PGDIND   DS    CL1                 REG/MISSD/MG INDICATOR                       
PGDSBRN  DS    CL1                 SORT BRAND                                   
PGDSSLN  DS    CL1                 SORT SPOT LENGTH                             
PGDSNO   DS    CL1                 SORT SPOT NUMBER                             
PGDELAD  DS    CL4                 ELEMENT ADDRESS                              
PGDFDAY  DS    CL1                                                              
PGDFNO   DS    CL2                                                              
PGDCUTIN DS    CL4                 A(CUTIN LIST)                                
PGDAFDAY DS    CL2                 AFFIDAVIT DAY                                
PGDAFTIM DS    CL2                 AFFIDAVIT TIME                               
PGD2BRN  DS    0C                  PIGGYBACK BRAND                              
PGD2SLN  DS    C                   PIGGYBACK SPOT LENGTH                        
PGDNOSP  DS    C                   NUMBER OF SPOTS                              
PGDEND   DS    0C                                                               
PGDLEN   EQU   PGDEND-PGSORT                                                    
PGDLN1   EQU   PGDELAD-PGLINNO                                                  
PGSRTLN  EQU   L'PGSORT                                                         
         SPACE 2                                                                
PGSRT1D  DSECT                                                                  
PGDS1WK  DS    CL2                                                              
PGDS1SLT DS    C                                                                
PGDS1BR  DS    CL1                                                              
PGDS1SL  DS    CL1                                                              
PGDS1IND DS    CL1                                                              
PGDS1CUT DS    CL4                                                              
PGDS1ADY DS    CL2                                                              
PGDS1ATM DS    CL2                                                              
         SPACE 2                                                                
PGSRT2D  DSECT                                                                  
PGDS2SLT DS    CL1                                                              
PGDS2WK  DS    CL2                                                              
PGDS2SNO DS    C                                                                
PGDS2BR  DS    CL1                                                              
PGDS2SL  DS    CL1                                                              
PGDS2IND DS    CL1                                                              
         EJECT                                                                  
PROFDSCT DSECT                  ***PROGRAM PROFILE 1 DSECT***                   
PROFMSG  DS    CL1                 SUPPRESS ALL CLEARED MESSAGE                 
PROFCNDS DS    CL1                 CONDENSE CONTROL                             
PROFSORT DS    CL1                 SORT CONTROL                                 
PROFCOM  DS    CL1                 COMMENT CONTROL                              
         EJECT                                                                  
PNAMD    DSECT                                                                  
PNDCODE  DS    CL1                 PROGRAM NUMBER                               
PNDNAME  DS    CL17                                                             
         SPACE 2                                                                
SEQSORT  DSECT                                                                  
SQSTART  DS    0C                                                               
SQ1DAY   DS    CL1                 DAY                                          
SQ1TIME  DS    CL2                 START-END QUARTER HOURS                      
SQ1PNUM  DS    CL1                 PROGRAM NUMBER                               
SQ1DADDR DS    CL4                 DISK ADDRESS                                 
SQ1END   DS    0C                                                               
         ORG   SQSTART                                                          
SQ2TIME  DS    CL2                                                              
SQ2DAY   DS    CL1                                                              
SQ2PNUM  DS    CL1                                                              
SQ2DADDR DS    CL4                                                              
SQ2END   DS    0C                                                               
         EJECT                                                                  
SUMDSECT DSECT                                                                  
SUMKEY   DS    0CL15                                                            
SUMCODE  DS    CL1                 X'90'                                        
SUMDPGNO DS    CL1                 DAYPART GROUP NO.                            
SUMDPGRP DS    CL3                 DAYPART GROUP CODE                           
SUMDPNO  DS    CL1                 DAYPART NO.                                  
SUMDPART DS    CL3                 DAYPART CODE                                 
SUMSLN   DS    CL1                 SPOT LENGTH                                  
SUMRTYP  DS    CL1                 1=WEEKLY,2=MONTHLY,3=PERIOD                  
SUMDT    DS    CL4                 START-END DATES(FFFF FOR TOTAL)              
SUMRPT   DS    CL1                 REPORT CODE                                  
SUMDATA  DS    0CL60                                                            
SUMSPOTS DS    CL4                 SPOTS                                        
SUMDL    DS    CL4                 DOLLARS                                      
SUMDLEQ  DS    CL4                 DOLLARS EQU                                  
SUMD1    DS    CL4                 DEMO 1                                       
SUMD1EQ  DS    CL4                 DEMO 1 EQU                                   
SUMD2    DS    CL4                 DEMO 2                                       
SUMD2EQ  DS    CL4                 DEMO 2 EQU                                   
SUMD3    DS    CL4                 DEMO 3                                       
SUMD3EQ  DS    CL4                 DEMO 3 EQU                                   
SUMD4    DS    CL4                 DEMO 4                                       
SUMD4EQ  DS    CL4                 DEMO 4 EQU                                   
SUMGDL   DS    CL4                 GOAL $                                       
SUMGDLE  DS    CL4                 GOAL $ EQU                                   
SUMGD1   DS    CL4                 GOAL DEMO                                    
SUMGD1E  DS    CL4                 GOAL DEMO EQU                                
         PRINT OFF                                                              
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPMEDBLOCK                                                     
       ++INCLUDE SPGENGOAL                                                      
       ++INCLUDE SPGENBUY                                                       
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPGENADD                                                       
       ++INCLUDE SPGENMKT                                                       
       ++INCLUDE SPGENREP                                                       
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE DDBUFFALOD                                                     
       ++INCLUDE SPMEDBDESD                                                     
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE DDREPMASTD                                                     
AGYRECD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'053SPREPI302 05/10/02'                                      
         END                                                                    
