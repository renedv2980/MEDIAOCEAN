*          DATA SET SPREPK102  AT LEVEL 016 AS OF 04/04/11                      
*PHASE SPK102A                                                                  
*INCLUDE WGTLIST                                                                
*INCLUDE PRINTER                                                                
*INCLUDE PRTREC                                                                 
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE SORTER                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE LOCKET                                                                 
         TITLE 'SPK102 - SPOTPAK RE-ALLOCATE/UNALLOCATION PROGRAM'              
SPK102   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPK102,RR=R5                                                   
*                                                                               
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         USING SPK102+4096,RC                                                   
*                                                                               
         ST    R5,RELO                                                          
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         L     R7,MEDBUFF                                                       
         USING MEDBLOCK,R7                                                      
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    UN10                                                             
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,CLTFRST                                                     
         BE    UN15                                                             
         CLI   MODE,PRDFRST                                                     
         BE    UN25                                                             
         CLI   MODE,ESTFRST                                                     
         BE    UN30                                                             
*                                                                               
         CLI   ESTERRFL,C'Y'                                                    
         BE    SPK20                                                            
*                                                                               
         CLI   MODE,PROCBUY                                                     
         BE    UN100                                                            
         CLI   MODE,MKTFRST                                                     
         BE    UN40                                                             
         CLI   MODE,STAFRST                                                     
         BE    UN50                                                             
         CLI   MODE,STALAST                                                     
         BE    UN200                                                            
         CLI   MODE,MKTLAST                                                     
         BE    UN210                                                            
         CLI   MODE,ESTLAST                                                     
         BE    UN220                                                            
         CLI   MODE,CLTLAST                                                     
         BE    UN230                                                            
*                                                                               
SPK20    DS    0H                                                               
         CLI   MODE,RUNLAST                                                     
         BE    UN240                                                            
EXIT     XMOD1 1                                                                
         EJECT                                                                  
*                                                                               
EQXIT    CR    RB,RB                                                            
         J     XIT                                                              
NEQXIT   LTR   RB,RB                                                            
XIT      XIT1                                                                   
*                                                                               
***********************************************************************         
* RUNFRST - FIRST FOR RUN                                             *         
***********************************************************************         
UN10     DS    0H                                                               
         L     RE,SSB                                                           
         OI    3(RE),X'08'         TURN ON FULL OFF-LINE RECOVERY               
*                                                                               
         STM   R7,RC,HDHKR7                                                     
         LA    R0,UNHDHK                                                        
         ST    R0,HEADHOOK                                                      
*                                                                               
         STM   R7,RC,SPHKR7                                                     
         LA    R0,UNSPHK                                                        
         ST    R0,SPOTHOOK                                                      
*                                                                               
         MVC   MEDNUMPE,=F'1'                                                   
         MVI   SPSUPMKT,C'Y'                                                    
         MVC   MEDLCHNK,=F'200'                                                 
*                                                                               
         MVI   SORTOPEN,C'N'                                                    
*                                                                               
         GOTO1 DATCON,DMCB,(0,QSTART),(2,BQSTARTP)    CONVT DATE                
         GOTO1 DATCON,DMCB,(0,QEND),(2,BQENDP)      TO COMPR FORM               
*                                                                               
         L     RF,VMASTC           SET RUN TIME FLAGS                           
         OC    MCREMPQK-MASTD(,RF),MCREMPQK-MASTD(RF)                           
         BZ    *+8                                                              
         OI    RUNFLAG,RUNFSOON    SET RUNNING SOON                             
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* REQFRST - FIRST FOR REQUEST                                         *         
***********************************************************************         
*                                                                               
REQF     TM    RUNFLAG,RUNFSOON    TEST RUNNING SOON                            
         JZ    EXIT                                                             
         OI    RUNFLAG,RUNFLOCK    SET MUST ISSUE UNLOCK AT RUNLAST             
*                                                                               
         USING LKKEYD,LOCKKEYW     AND BUILD LOCK KEY                           
         XC    LOCKEY,LOCKEY                                                    
         L     RF,VMASTC                                                        
         L     RF,MCUTL-MASTD(RF)                                               
         MVC   LOCKSE,4(RF)                                                     
         MVC   LOCKAGY,QAGY                                                     
         MVC   LOCKRTY,=AL2(LKBARTYQ)                                           
         MVC   LOCKKEY,SPACES                                                   
         MVC   LKBAMED,QMED                                                     
         MVC   LKBACLT,QCLT                                                     
         CLC   QEST,=C'ALL  '      FOR A MULTIPLE ESTIMATE REQUEST              
         JE    EXIT                ALL ESTIMATES WERE LOCKED                    
         CLC   =C'NO',QEST                                                      
         JE    EXIT                                                             
         CLI   QESTEND,C' '                                                     
         JNE   EXIT                                                             
         MVC   LKBAEST,QEST        ELSE A SINGLE ESTIMATE WAS LOCKED            
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* CLTFRST - CLIENT FIRST                                              *         
***********************************************************************         
*                                                                               
UN15     DS    0H                                                               
         MVI   QOPT5,C'N'          DISSALLOW PRODUCT CHANGE REPORT              
*                                  (FOR NOW)                                    
         MVC   P,SPACES                                                         
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         MVI   RCSUBPRG,0          SUBPRG = NUMBER OF RE-ALLOC PRDS             
*                                                                               
         CLC   Q2USER(6),=C'DELETE'      IF WE'RE NOT DELETING SPOTS,           
         BE    UN20                                                             
         CLC   Q2USER(9),=C'UNMATCHED'                                          
         BE    UN20                                                             
         L     R6,ADCLT            DISSALOW BRAND POL CLIENT                    
         USING CLTHDR,R6                                                        
         CLI   CPROF,C'0'                                                       
         BNE   CLTERR                                                           
         DROP  R6                                                               
* DO NOT ALLOW CANADIAN MEDIA C OR N                                            
UN20     L     R6,ADAGY                                                         
         USING AGYHDR,R6                                                        
         CLI   AGYPROF+7,C'C'      TEST CANADIAN AGENCY                         
         BNE   UN20A                                                            
*        CLI   QMED,C'N'           ALLOW N FOR ALLOCATIONS                      
*        BE    CNCLTERR                                                         
         CLI   QMED,C'C'           BUT NOT C                                    
         BE    CNCLTERR                                                         
*                                                                               
         CLC   Q2USER(6),=C'DELETE'       NO N FOR DELETE OPTIONS               
         BE    *+14                       BECAUSE IT DOESN'T WORK!              
         CLC   Q2USER(9),=C'UNMATCHED'                                          
         BNE   UN20A                                                            
         CLI   QMED,C'N'                                                        
         BE    CNDELERR                                                         
*                                                                               
UN20A    CLC   QPRD,=C'POL'        TEST PRD=POL                                 
         BNE   *+14                                                             
         CLC   QAREA+49(3),SPACES  YES-CHECK PARTNER PRD ISN'T REQ'STED         
         BH    PRDERR                                                           
         XC    TOTCLT,TOTCLT                                                    
         XC    BRDTAB,BRDTAB                                                    
         XC    BRDLIST,BRDLIST                                                  
         MVI   DELUNMAT,C'N'                                                    
         MVI   DELSPOTS,C'N'                                                    
         CLC   Q2USER(3),SPACES    ALLOW SPACES FOR UNALLOCATE                  
         BE    UN24D                                                            
         CLC   Q2USER(6),=C'DELETE'                                             
         BNE   *+12                                                             
         MVI   DELSPOTS,C'Y'                                                    
         B     UN24D                                                            
         CLC   Q2USER(9),=C'UNMATCHED'                                          
         BNE   *+12                                                             
         MVI   DELUNMAT,C'Y'       DELETE UNMATCHED SPOTS                       
         B     UN24D                                                            
         SPACE 1                                                                
* EDIT LIST OF PRDS TO BE ALLOCATED                                             
         LA    R4,BRDLIST                                                       
         USING TOPRDD,R4                                                        
         LA    R5,Q2USER                                                        
         LA    R3,MAXBRDS          UP TO 5 PRODUCTS                             
         MVI   LASTBRD,C'N'                                                     
*                                                                               
UN21     DS    0H                                                               
         IC    RE,RCSUBPRG                                                      
         LA    RE,1(RE)            INCREMENT PRD COUNT                          
         STC   RE,RCSUBPRG                                                      
         CHI   R3,1                                                             
         BNE   *+8                                                              
         MVI   LASTBRD,C'Y'                                                     
         LA    R0,2                                                             
         LR    R6,R5                                                            
         LA    R2,TOPRD1                                                        
         LA    R8,TOSLN1                                                        
*                                                                               
UN22     CLC   0(3,R6),SPACES                                                   
         BNH   UN24B                                                            
         L     R1,ADCLT                                                         
         LA    R1,CLIST-CLTHDR(R1)                                              
*                                                                               
UN23     CLC   0(3,R6),0(R1)                                                    
         BE    UN24                                                             
         LA    R1,4(R1)                                                         
         CLI   0(R1),C' '                                                       
         BH    UN23                                                             
         DC    H'0'                                                             
*                                                                               
UN24     MVC   0(1,R2),3(R1)       MOVE PRD CODE                                
         CLI   LASTBRD,C'Y'        NO SPOT LENGTH FOR LAST BRAND                
         BE    UN24A                                                            
         CLC   6(3,R6),SPACES      TEST SPOT LENGTH GIVEN                       
         BNH   UN24A                                                            
         LA    R1,6(R6)                                                         
         BAS   RE,GETLEN           EXTRACT THE SPOT LENGTH                      
         STC   RF,0(R8)                                                         
*                                                                               
UN24A    LA    R6,3(R6)                                                         
         LA    R2,TOPRD2                                                        
         LA    R8,TOSLN2                                                        
         BCT   R0,UN22                                                          
*                                                                               
UN24B    MVC   TOWGT,12(R5)        MOVE THE WEIGHT                              
         CLI   LASTBRD,C'Y'                                                     
         BNE   *+10                                                             
         MVC   TOWGT,6(R5)                                                      
         CLI   TOWGT,C' '                                                       
         BNE   *+8                                                              
         MVI   TOWGT,1             SET DEFAULT WEIGHT                           
         TM    TOWGT,X'F0'         TEST EBCDIC VALUE                            
         BNO   *+8                 NO                                           
         NI    TOWGT,X'0F'         DROP SIGN BITS                               
*                                                                               
         LA    R4,TOPRDL(R4)       NEXT PRODUCT                                 
         LA    R5,13(R5)                                                        
         CLC   0(3,R5),SPACES                                                   
         BE    *+8                                                              
         BCT   R3,UN21                                                          
*                                                                               
         LA    R1,1                SET UP BRAND LIST FOR WGTLIST RTN            
         LA    R3,MAXBRDS                                                       
         LA    R4,BRDLIST                                                       
         LA    R5,WORK                                                          
         XC    WORK,WORK                                                        
*                                                                               
UN24C    STC   R1,0(R5)                                                         
         MVC   1(1,R5),TOWGT                                                    
         LA    R1,1(R1)                                                         
         LA    R4,TOPRDL(R4)                                                    
         LA    R5,2(R5)                                                         
         CLI   TOPRD1,0                                                         
         BE    *+8                                                              
         BCT   R3,UN24C                                                         
*                                                                               
* NOW BUILD WEIGHTED BRAND SELECTION TABLE                                      
*                                                                               
         GOTO1 =V(WGTLIST),DMCB,WORK,BRDTAB,XSORT,RR=RELO                       
*                                                                               
UN24D    XC    PRD2,PRD2                                                        
         MVI   BPRD2,0                                                          
         MVI   QSLNT,0                                                          
         MVI   QSLN1,0                                                          
         MVI   QSLN2,0                                                          
         CLC   QAREA+49(3),SPACES  TEST PARTNER PRODUCT                         
         BNH   UN24F                                                            
         MVC   PRD2,QAREA+49       YES-                                         
         L     R6,ADCLT            GET ITS CODE                                 
         USING CLTHDR,R6                                                        
         LA    R6,CLIST                                                         
         DROP  R6                                                               
*                                                                               
UN24E    CLC   QAREA+49(3),0(R6)                                                
         BNE   *+14                                                             
         MVC   BPRD2,3(R6)                                                      
         B     UN24F                                                            
         LA    R6,4(R6)                                                         
         CLI   0(R6),C' '                                                       
         BH    UN24E                                                            
         DC    H'0'                                                             
*                                                                               
UN24F    XC    KEY,KEY             READ PRODUCT RECORD TO GET NAME              
         LA    R6,KEY                                                           
         USING PRDHDRD,R6                                                       
         MVC   PKEYAM,BAGYMD                                                    
         MVC   PKEYCLT,BCLT                                                     
         MVC   PKEYPRD,PRD2                                                     
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,ADPRD                                                         
         ST    R6,AREC                                                          
         GOTO1 GET                                                              
         MVC   PRDNM2,SPACES                                                    
         MVC   PRDNM2(L'PNAME),PNAME                                            
         DROP  R6                                                               
*                                                                               
         CLC   QAREA+52(3),SPACES  TEST SPOT LENGTH FILTER                      
         BNH   UN24G                                                            
         LA    R1,QAREA+52                                                      
         BAS   RE,GETLEN           YES                                          
         LR    R6,RF                                                            
         STC   R6,QSLN1                                                         
         STC   R6,QSLNT                                                         
         CLI   BPRD2,0             TEST PARTNER PRODUCT                         
         BE    UN24G                                                            
         CLC   QAREA+55(3),SPACES  AND 2ND SPOT LENGTH FILTER                   
         BNH   UN24G                                                            
         LA    R1,QAREA+55                                                      
         BAS   RE,GETLEN           YES                                          
         STC   RF,QSLN2                                                         
         AR    R6,RF                                                            
         STC   R6,QSLNT            SET TOTAL LENGTH                             
*                                                                               
UN24G    CLI   QOPT4,C'Y'          TEST INCLUDE PIGGYBACKS OPTION IS ON         
         BNE   UN24X                                                            
         CLI   BPRD2,0             YES-CHECK THAT THERE ARE NO                  
         BE    *+6                     SECONDARY PRODUCTS REQUESTED             
         DC    H'0'                    (REQUEST PROGRAM SHOULD HAVE             
         LA    R4,BRDLIST               SCREENED THESE OUT)                     
         USING TOPRDD,R4                                                        
         LA    R0,MAXBRDS                                                       
         CLI   TOPRD2,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R4,TOPRDL(R4)                                                    
         BCT   R0,*-14                                                          
*                                                                               
UN24X    B     EXIT                                                             
*                                                                               
         SPACE 2                                                                
PRDERR   MVC   P(L'PRDMSG),PRDMSG                                               
         GOTO1 REPORT                                                           
         GOTO1 AENDREQ                                                          
PRDMSG   DC    C'** ERROR ** PARTNER PRODUCT IS INVALID'                        
*                                                                               
CLTERR   MVC   P(L'CLTMSG),CLTMSG                                               
         GOTO1 REPORT                                                           
         GOTO1 AENDREQ                                                          
CLTMSG   DC    C'** ERROR ** MAY NOT REALLOCATE BRAND POOL CLIENT'              
*                                                                               
CNCLTERR MVC   P(L'CNCLTMSG),CNCLTMSG                                           
         GOTO1 REPORT                                                           
         GOTO1 AENDREQ                                                          
CNCLTMSG DC    C'** ERROR ** MAY NOT RE-ALLOCATE MEDIA C'                       
*                                                                               
CNDELERR MVC   P(L'CNDELMSG),CNDELMSG                                           
         GOTO1 REPORT                                                           
         GOTO1 AENDREQ                                                          
CNDELMSG DC    C'** ERROR ** DELETE NOT AVAILABLE FOR MEDIA N'                  
*                                                                               
GETLEN   LA    RF,2                GET SPOT LENGTH                              
         CLI   2(R1),C'0'          R1=A(3-BYTE LENGTH)                          
         BNL   GETLEN2                                                          
         BCTR  RF,0                                                             
         CLI   1(R1),C'0'                                                       
         BNL   GETLEN2                                                          
         BCTR  RF,0                                                             
GETLEN2  EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R1)                                                      
         CVB   RF,DUB              RETURN LENGTH IN RF                          
         BR    RE                                                               
         EJECT                                                                  
* PRDFRST                                                                       
*                                                                               
UN25     CLI   QOPT5,C'Y'          FOR PRODUCT CHANGE REPORT,                   
         BNE   EXIT                GET ALL ESTIMATE NAMES                       
         LA    R1,ESTNAMES                                                      
         LA    RF,255                                                           
         XC    0(L'ESTNAMES,R1),0(R1)                                           
         LA    R1,L'ESTNAMES(R1)                                                
         BCT   RF,*-10                                                          
         LA    R6,KEY                                                           
         XC    KEY,KEY                                                          
         USING ESTHDRD,R6                                                       
         MVC   EKEYAM,BAGYMD                                                    
         MVC   EKEYCLT,BCLT                                                     
         MVC   EKEYPRD,PRD                                                      
*                                                                               
UN26     GOTO1 HIGH                                                             
         B     UN28                                                             
*                                                                               
UN27     GOTO1 SEQ                                                              
*                                                                               
UN28     CLC   KEY(EKEYEST-EKEY),KEYSAVE                                        
         BNE   EXIT                                                             
         LA    R6,KEY                                                           
         CLI   EKEYEST+1,0                                                      
         BE    *+14                                                             
         MVC   EKEYEST+1(5),=X'FFFFFFFFFF'                                      
         B     UN26                                                             
         L     R6,ADEST                                                         
         ST    R6,AREC                                                          
         GOTO1 GET                                                              
         ZIC   R1,EKEYEST                                                       
         BCTR  R1,0                                                             
         MH    R1,=Y(L'ESTNAMES)                                                
         LA    R1,ESTNAMES(R1)                                                  
         MVC   0(L'ESTNAMES,R1),EDESC                                           
         B     UN27                                                             
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
* ESTFRST                                                                       
*                                                                               
UN30     DS    0H                                                               
         L     RF,ADEST                                                         
         USING ESTHDR,RF                                                        
         CLI   EOWSDAY,0           IF OUT OF WEEK START DAY PRESENT             
         BE    *+10                                                             
         MVC   SPOTPROF+8(1),EOWSDAY   ADJUST SPOTPROF                          
         DROP  RF                                                               
*                                                                               
         MVI   ESTERRFL,C'N'                                                    
*                                                                               
*        BRAS  RE,ESTVAL                                                        
*        BE    *+8                                                              
*        MVI   ESTERRFL,C'Y'                                                    
*                                                                               
         XC    TOTEST,TOTEST                                                    
         MVI   MEDBRAND,220        GET ALL SPOTS (ALLOC AND UNALL)              
         CLI   DELUNMAT,C'Y'                                                    
         BE    *+18                                                             
         CLC   QPRD,=C'POL'        TEST QPRD=POL                                
         BNE   *+8                                                              
         MVI   MEDBRAND,219        YES-ONLY GET UNALLOCATED                     
* CLEAR ESTIMATE TABLE                                                          
         LA    R0,4                                                             
*        LA    R1,ESTTAB                                                        
         L     R1,=A(ESTTAB)                                                    
         XC    0(256,R1),0(R1)                                                  
         LA    R1,256(R1)                                                       
         BCT   R0,*-10                                                          
*                                                                               
         GOTO1 MEDDATE,DMCB,(RA)                                                
*                                                                               
         CLI   QOPT5,C'Y'          GET PRODUCT NAMES FOR PRODUCT                
         BNE   EXIT                CHANGE REPORT                                
         MVC   SVQPRD,QPRD                                                      
         MVC   QPRD,=C'ALL'                                                     
         GOTO1 MEDPRDRD,DMCB,(RA)                                               
*        DC    H'0'                                                             
         MVC   QPRD,SVQPRD                                                      
*                                                                               
         B     EXIT                                                             
         SPACE 2                                                                
* MKTFRST                                                                       
*                                                                               
UN40     DS    0H                                                               
         XC    TOTMKT,TOTMKT                                                    
         MVI   MKTSW,C'N'          RESET MKT NAME PRINTED                       
         MVI   STASW,C'N'          RESET STATION PRINTED                        
*                                                                               
         LA    R0,BRDTAB                                                        
         ST    R0,NEXTBRD                                                       
*                                                                               
         B     EXIT                                                             
         SPACE 2                                                                
* STAFRST                                                                       
*                                                                               
UN50     DS    0H                                                               
         XC    TOTSTA,TOTSTA                                                    
         MVI   STASW,C'N'          RESET STATION PRINTED                        
         L     R6,ADSTAT                                                        
         USING STARECD,R6                                                       
         MVC   SVSYSNM,SSYSNAME    SAVE CABLE SYSTEM NAME (IF ANY)              
         B     EXIT                                                             
         DROP  R6                                                               
*                                                                               
* PROCBUY                                                                       
*                                                                               
UN100    DS    0H                                                               
         MVI   SKIPBUY,C'N'                                                     
*                                                                               
         L     R6,ADBUY                                                         
         USING BUYREC,R6                                                        
*                                                                               
         CLC   4(2,R6),KEY+4       TEST SAME MARKET (SPILL)                     
         BNE   EXIT                NO - SKIP SPILL BUYS                         
         CLI   QSLNT,0             TEST SPOT LENGTH FILTER                      
         BE    *+14                                                             
         CLC   BDSEC,QSLNT         YES                                          
         BNE   EXIT                                                             
*                                                                               
* FILTER BY DAYPART                                                             
         CLI   QOPT3,C' '                                                       
         BNH   *+14                                                             
         CLC   BDDAYPT,QOPT3                                                    
         BNE   EXIT                                                             
*                                                                               
         LA    R4,BRDLIST                                                       
         CLI   1(R4),X'00'         IS THERE SECOND PRODUCT?                     
         BE    UN101               NO - DON'T BOTHER                            
         CLC   2(2,R4),=X'0000'    LENGTH GIVEN?                                
         BNE   UN101               YES - SKIP                                   
         TM    BDSEC,X'01'         SPOT LENGTH EVEN?                            
         BNO   UN101                                                            
         MVC   P(60),=CL60'ERROR: SPOT LENGTH NOT EVEN'                         
         GOTO1 REPORT                                                           
         MVC   P(20),=CL20'SKIPPING BUY RECORD '                                
         MVC   P+20(3),CLT                                                      
         MVC   P+24(3),PRD                                                      
         MVC   P+28(3),EST                                                      
         MVC   P+32(4),MKT                                                      
         MVC   P+37(5),STA                                                      
*                                                                               
         LLC   R0,BUYKBUY                                                       
         TM    BUYRCNTL,BUYRLN2    2-BYTE LINE NUMBERS?                         
         BZ    *+8                                                              
         ICM   R0,3,BUYRLIN                                                     
         EDIT  (R0),(3,P+43)                                                    
*                                                                               
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         DROP  R6                                                               
*                                                                               
UN101    DS    0H                                                               
         XC    TOTBUY,TOTBUY                                                    
         XC    ADDLIST,ADDLIST     CLEAR ADDED PRD LIST                         
         MVI   ERRFLAG,C'N'        AND RESET ERROR FLAGS                        
         MVI   ERRFLAG2,C'N'                                                    
         MVI   REPSW,C'N'          RESET ACTIVITY FLAG                          
         MVI   FRSTPRD1,0                                                       
         MVI   FRSTPRD2,0                                                       
*                                                                               
         GOTO1 MEDGETBY,DMCB,(RA),0                                             
*                                                                               
         CLC   =C'DELETE',Q2USER                                                
         BE    *+12                                                             
         CLI   DELUNMAT,C'Y'                                                    
         BNE   UN104                                                            
         L     R6,ADBUY            REMOVE ELEMENTS WITH ELCODE X'7F'            
         LA    R6,24(R6)                                                        
         MVI   ELCDLO,X'7F'                                                     
         MVI   ELCDHI,X'7F'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   UN104                                                            
         MVI   REPSW,C'Y'          SET ACTIVE                                   
*                                                                               
UN102    DS    0H                                                               
         GOTO1 RECUP,DMCB,ADBUY,(R6)                                            
         BAS   RE,NEXTEL2                                                       
         BE    UN102                                                            
*                                                                               
UN104    CLI   REPSW,C'Y'          TEST ACTIVITY THIS LINE                      
         BNE   EXIT                                                             
*                                                                               
         L     R6,ADBUY                                                         
         USING BUYREC,R6                                                        
         CLI   DELSPOTS,C'Y'                                                    
         BE    UN110                                                            
         CLI   DELUNMAT,C'Y'                                                    
         BE    UN110                                                            
         CLI   RCSUBPRG,0          TEST UNALL REQUEST                           
         BNE   *+14                                                             
         XC    BDMASPRD,BDMASPRD   YES-CLEAR THE MASTER PRODUCT                 
         B     UN110                                                            
         CLI   FRSTPRD1,0          TEST HAVE A FIRST RE-ALLOCATION PRD          
         BE    UN110                                                            
         CLC   QPRD,=C'POL'        YES-TEST ALLOCATION REQUEST                  
         BNE   *+16                                                             
         CLI   BDMASPRD,0          YES-ONLY SET MASPRD IF PREVIOUSLY 0          
         BE    UN108                                                            
         B     UN110                                                            
         CLI   QOPT4,C'Y'          TEST 'INCLUDE PIGGYBACKS' OPTION             
         BNE   UN106                                                            
         LA    R1,BDMASPRD         YES-ONLY CHANGE MASPRD IF REQUESTED          
         CLC   BPRD,0(R1)              PRODUCT IS IN IT                         
         BE    *+18                                                             
         LA    R1,1(R1)                                                         
         CLC   BPRD,0(R1)                                                       
         BNE   UN110                                                            
         MVC   0(1,R1),FRSTPRD1                                                 
         B     UN110                                                            
*                                                                               
UN106    CLC   BPRD,BDMASPRD       FOR RE-ALLOCATION, ONLY SET MASPRD           
         BNE   UN110               IF IT CURRENTLY IS THE REQUESTED PRD         
         CLC   BPRD2,BDMASPRD+1                                                 
         BNE   UN110                                                            
*                                                                               
UN108    MVC   BDMASPRD(1),FRSTPRD1                                             
         MVC   BDMASPRD+1(1),FRSTPRD2                                           
         DROP  R6                                                               
*                                                                               
UN110    OC    ADDLIST,ADDLIST     ANY NEW PRDS                                 
         BZ    *+12                                                             
         LA    R0,ADDLIST          IF SO, SET ADDRESS OF LIST IN DM6            
         ST    R0,DM6                                                           
*                                                                               
         CLI   SKIPBUY,C'Y'                                                     
         BNE   UN111                                                            
*                                                                               
         GOTO1 REPORT                                                           
         MVI   P,C'*'                                                           
         MVC   P+1(79),P                                                        
         GOTO1 REPORT                                                           
         MVI   P,C'*'                                                           
         MVC   P+1(79),P                                                        
         MVC   P+5(30),=CL30' ERROR: ALLOCATION FAILURE  '                      
         GOTO1 REPORT                                                           
         MVI   P,C'*'                                                           
         MVC   P+1(79),P                                                        
         MVC   P+5(30),=CL30'        BUY RECORD TOO BIG  '                      
         GOTO1 REPORT                                                           
         MVI   P,C'*'                                                           
         MVC   P+1(79),P                                                        
         MVC   P+5(35),=CL35'        PLEASE, SPLIT THE BUYLINE'                 
         GOTO1 REPORT                                                           
         MVI   P,C'*'                                                           
         MVC   P+1(79),P                                                        
         GOTO1 REPORT                                                           
         MVC   P(40),=CL40'SKIPPING THE FOLLOWING BUYLINE:'                     
         GOTO1 REPORT                                                           
         BAS   RE,UNPRTLN          PRINT BUYLINE DATA                           
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         XC    DM6(4),DM6          CLEAR PRD LIST ADDR                          
         B     EXIT                                                             
*                                                                               
UN111    DS    0H                                                               
         MVC   AREC,ADBUY                                                       
         GOTO1 PUT                                                              
         MVC   KEY1,KEY            SAVE KEY FOR SEQ READ                        
*                                                                               
         CLC   =C'TRACE ',QUESTOR                                               
         BNE   *+8                                                              
         BAS   RE,UNTRACE                                                       
*                                                                               
         MVI   HASCUTIN,C' '                                                    
         L     RF,ADAGY                                                         
         CLI   AGYPROF+7-AGYHDR(RF),C'C'                                        
         BNE   CONT                                                             
         CLI   QMED,C'N'                                                        
         BNE   CONT                                                             
         GOTO1 =A(XPLODE)          EXPLODE ALLOCS TO LOCAL STATIONS             
*                                                                               
CONT     XC    DM6(4),DM6          CLEAR PRD LIST ADDR                          
         MVC   KEY,KEY1            RESTORE KEY                                  
         GOTO1 HIGH                RESTORE DIR FOR SEQ READ                     
*                                                                               
         BAS   RE,UNPRTLN          PRINT BUYLINE DATA                           
         LA    R1,TOTBUY                                                        
         BAS   RE,UNFMT                                                         
         GOTO1 REPORT                                                           
*                                                                               
         CLI   STASW,C'Y'          TEST STATION FIRST                           
         BE    UN112                                                            
         CLC   SVSYSNM,SPACES      YES-PRINT CABLE SYSTEM NAME (IF ANY)         
         BNH   UN112                                                            
         LA    R4,P                                                             
         USING UNLINED,R4                                                       
         MVC   UNSTA(L'SVSYSNM),SVSYSNM                                         
         GOTO1 REPORT                                                           
*                                                                               
UN112    MVI   MKTSW,C'Y'                                                       
         MVI   STASW,C'Y'                                                       
         SPACE 2                                                                
* ADD BUY TO OTHER TOTALS                                                       
         LA    R4,TOTSTA                                                        
UN120    LA    R0,MAXBRDS+1                                                     
         LA    R1,TOTBUY                                                        
UN122    LM    RE,RF,0(R4)                                                      
         A     RE,0(R1)            SPOTS                                        
         A     RF,4(R1)            DOLLARS                                      
         STM   RE,RF,0(R4)                                                      
         LA    R1,8(R1)                                                         
         LA    R4,8(R4)                                                         
         BCT   R0,UN122                                                         
*                                                                               
         LA    R0,TOTCLT                                                        
         CR    R4,R0                                                            
         BNH   UN120                                                            
*                                                                               
         XC    TOTBUY,TOTBUY                                                    
         B     EXIT                                                             
*                                                                               
* TRACE ROUTINE FOR TESTING                                                     
*                                                                               
UNTRACE  NTR1                                                                   
*                                                                               
         GOTO1 REPORT              SKIP A LINE                                  
         L     R6,ADBUY                                                         
         SR    R0,R0                                                            
         ICM   R0,3,13(R6)                                                      
*                                                                               
         GOTO1 =V(PRTREC),DMCB,(C'E',ADBUY),(24,13),V(PRINT),          X        
               V(HEXOUT)                                                        
*                                                                               
*        GOTO1 PRNTBL,TRDMCB,=C'BUYREC',ADBUY,C'DUMP',                          
*              (R0),=C'1D',RR=RELO                                              
         GOTO1 REPORT              SKIP A LINE                                  
         GOTO1 PRNTBL,TRDMCB,=C'ADDED PRD LIST',ADDLIST,C'DUMP',       X        
               128,=C'1D',RR=RELO                                               
         GOTO1 REPORT              SKIP A LINE                                  
         B     EXIT                                                             
*                                                                               
*                                                                               
*                                                                               
TRDMCB   DS    6F                                                               
*                                                                               
* SPOTHOOK    R1 = A(GETRATE AREA)                                              
*                                                                               
         CNOP  0,4                                                              
         USING *,RF                                                             
UNSPHK   NTR1                                                                   
         LM    R7,RC,SPHKR7                                                     
         B     SPHK2                                                            
SPHKR7   DC    6F'0'                                                            
         DROP  RF                                                               
*                                                                               
SPHK2    DS    0H                                                               
         CLI   SKIPBUY,C'Y'                                                     
         BE    SPHKX                                                            
*                                                                               
         L     RE,0(R1)            POINT TO GETRATE AREA                        
         MVC   DUB,0(RE)           SAVE SPOTS AND DOLLARS                       
         L     R6,SPOTADDR                                                      
         TM    6(R6),X'C0'         IGNORE MINUS OR MINUSSED SPOTS               
         BNZ   SPHKX                                                            
         OC    4(2,R6),4(R6)       TEST PAID                                    
         BNZ   SPHKX                                                            
*                                                                               
         CLI   DELUNMAT,C'Y'       TEST DELETE UNMATCHED                        
         BNE   SPHK6                                                            
         CLC   QPRD,=C'POL'        TEST POL REQUEST                             
         BE    SPHK4               YES-ACCEPT ALL                               
         CLI   1(R6),14            NO-TEST SINGLE PRD ALLOCATION                
         BNE   SPHK3                                                            
         CLI   BPRD2,0             YES-IGNORE IF PIGGYBACK REQUEST              
         BNE   SPHKX                                                            
         CLC   BPRD,10(R6)         RIGHT BRAND                                  
         BNE   SPHKX                                                            
         B     SPHK4                                                            
*                                                                               
SPHK3    CLI   1(R6),18            TEST PIGGYBACK                               
         BNE   SPHKX                                                            
         CLI   BPRD2,0             TEST PIGGYBACK REQUESTED                     
         BE    SPHKX               NO-IGNORE                                    
         CLC   BPRD,10(R6)         EACH PRODUCT MUST MATCH                      
         BNE   SPHKX                                                            
         CLC   BPRD2,14(R6)                                                     
         BNE   SPHKX                                                            
*                                                                               
SPHK4    SR    R0,R0               LOOK FOR AFFID ELEMENT                       
         LR    R1,R6                                                            
*                                                                               
SPHK5    IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         CLI   0(R1),0                                                          
         BE    SPHK9                                                            
         CLI   0(R1),X'0B'                                                      
         BL    SPHK5                                                            
         CLI   0(R1),X'0D'                                                      
         BNH   SPHK9                                                            
         CLI   0(R1),X'10'                                                      
         BE    SPHKX               THERE IS ONE - DON'T DELETE                  
         B     SPHK5                                                            
*                                 IGNORE MATCHED SPOTS, AS PER BUY PROG         
SPHK6    CLC   Q2USER(6),=C'DELETE'  DON'T MESS WITH DELETE SPOTS LOGIC         
         BE    SPHK6B                                                           
         SR    R0,R0               LOOK FOR AFFID ELEMENT                       
         LR    RF,R6                                                            
SPHK6A   IC    R0,1(RF)                                                         
         AR    RF,R0                                                            
         CLI   0(RF),0                                                          
         BE    SPHK6B                                                           
         CLI   0(RF),X'0B'                                                      
         BL    SPHK6A                                                           
         CLI   0(RF),X'0D'                                                      
         BNH   SPHK6B                                                           
         CLI   0(RF),X'10'                                                      
         BE    SPHKX               FOUND SO SPOT MATCHED - IGNORE               
         B     SPHK6A                                                           
*                                                                               
SPHK6B   CLC   QPRD,=C'POL'        TEST QPRD=POL                                
         BNE   *+16                                                             
         CLI   1(R6),10            YES-ONLY ACCEPT UNALL SPOTS                  
         BE    SPHK9                                                            
         B     SPHKX                                                            
*                                                                               
         CLI   1(R6),10                                                         
         BNH   SPHKX               IGNORE UNALL SPOTS                           
*                                                                               
         OC    12(2,R6),12(R6)     TEST DETAIL BILLED                           
         BNZ   SPHKX               YES  - IGNORE                                
         CLI   1(R6),14            TEST SINGLE BRAND ALLOCATED                  
         BNE   SPHK7                                                            
         CLI   BPRD2,0             YES-IGNORE IF PIGGYBACK REQUEST              
         BNE   SPHKX                                                            
         CLC   BPRD,10(R6)         RIGHT BRAND                                  
         BNE   SPHKX                                                            
         B     SPHK9                                                            
*                                                                               
SPHK7    CLI   1(R6),18            TEST PIGGYBACK                               
         BNE   SPHKX                                                            
         OC    16(2,R6),16(R6)     YES-IGNORE BILLED                            
         BNZ   SPHKX                                                            
         CLI   BPRD2,0             TEST PIGGYBACK REQUESTED                     
         BNE   SPHK8               YES                                          
         CLI   QOPT4,C'Y'          NO-TEST TO INCLUDE PIGGYBACKS                
         BNE   SPHKX               NO                                           
         CLC   10(1,R6),BPRD       YES-THEN ONE OF THE PRODUCTS MUST            
         BE    SPHK9                   MATCH                                    
         CLC   14(1,R6),BPRD                                                    
         BE    SPHK9                                                            
         B     SPHKX                                                            
*                                                                               
SPHK8    CLC   BPRD,10(R6)         EACH PRODUCT MUST MATCH                      
         BNE   SPHKX                                                            
         CLC   BPRD2,14(R6)                                                     
         BNE   SPHKX                                                            
         CLI   QSLN2,0             TEST 2ND LENGTH FILTER                       
         BE    SPHK9                                                            
         CLC   QSLN1,11(R6)        YES-1ST AND 2ND LENGTHS MUST MATCH           
         BNE   SPHKX                                                            
         CLC   QSLN2,15(R6)                                                     
         BNE   SPHKX                                                            
*                                                                               
SPHK9    MVI   REPSW,C'Y'          SET ACTIVITY FLAG                            
         CLI   RCSUBPRG,0          TEST UNALL OR DELETE REQUEST                 
         BE    SPHK42                                                           
*                                                                               
         L     R3,NEXTBRD          INSERT NEW PRODUCT(S) IN ELEM IF             
*                                  ESTIMATE IS ON FILE                          
SPHK10   ZIC   R4,0(R3)                                                         
         BCTR  R4,0                                                             
         MH    R4,=Y(TOPRDL)                                                    
         LA    R4,BRDLIST(R4)                                                   
         USING TOPRDD,R4                                                        
         MVC   FRSTPRD1,TOPRD1     RECORD FIRST RE-ALLOCATION PRODUCTS          
         MVC   FRSTPRD2,TOPRD2                                                  
*                                                                               
         LA    R0,2                                                             
         LA    R8,TOPRD1           CHECK THE ESTIMATES                          
*                                                                               
SPHK12   CLC   BPRD,0(R8)                                                       
         BE    SPHK14                                                           
         IC    R5,0(R8)                                                         
*        GOTO1 =V(SPESTCHK),HKDMCB,((R5),ADBUY),ESTTAB,(RA),RR=RELO             
         GOTO1 =V(SPESTCHK),HKDMCB,((R5),ADBUY),A(ESTTAB),(RA),RR=RELO          
         CLI   8(R1),0                                                          
         BNE   SPHKERR                                                          
*                                                                               
SPHK14   LA    R8,TOPRD2                                                        
         CLI   0(R8),0                                                          
         BE    *+8                                                              
         BCT   R0,SPHK12                                                        
*                                                                               
         XC    WORK,WORK           BUILD NEW SPOT ELEMENT                       
         ZIC   RF,1(R6)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(R6)                                                    
         CLI   QOPT4,C'Y'          TEST 'INCLUDE PIGGYBACKS' OPTION             
         BNE   SPHK18                                                           
***                                                                             
         CLI   1(R6),18            YES-TEST PIGGYBACK SPOT                      
         BE    SPHK15                                                           
         MVC   WORK+10(1),TOPRD1   NO-JUST REPLACE THE PRODUCT                  
         MVI   WORK+1,X'0E'        UPDATE ELEMENT LENGTH                        
         L     R1,ADBUY                                                         
         ZIC   RE,BDSEC-BUYREC(R1) RE=BUY SPOT LENGTH                           
         STC   RE,WORK+11          UPDATE SPOT LENGTH                           
         B     SPHK22                                                           
*                                                                               
SPHK15   DS    0H                                                               
         CLC   BPRD,10(R6)         YES-REPLACE EITHER 1ST OR 2ND PRD            
         BNE   SPHK15A                 WITH TO PRODUCT                          
         MVC   WORK+10(1),TOPRD1                                                
         LA    R1,WORK+11                                                       
         B     SPHK16                                                           
*                                                                               
SPHK15A  DS    0H                                                               
         CLC   BPRD,14(R6)                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   WORK+14(1),TOPRD1                                                
         LA    R1,WORK+15                                                       
*                                                                               
SPHK16   CLC   WORK+10(1),WORK+14  TEST 1ST PRD = 2ND PRD                       
         BE    SPHKERR2            YES-ERROR                                    
*                                                                               
         MVC   BYTE,0(R1)          GET COST FOR REPLACED PART OF ELEM           
         GOTO1 GETRATE,HKDMCB,(BPRD,COSTAREA),(BYTE,ADBUY),(0,(R6))             
         MVC   DUB,COSTAREA                                                     
         B     SPHK22                                                           
*                                                                               
SPHK18   MVI   WORK+1,14                                                        
         MVC   WORK+10(1),TOPRD1   FIRST PRODUCT                                
         L     R1,ADBUY                                                         
         ZIC   RE,BDSEC-BUYREC(R1) RE=BUY SPOT LENGTH                           
         CLI   TOPRD2,0                                                         
         BNE   *+12                                                             
         STC   RE,WORK+11          ONE PRODUCT - FILL IN SPOT LENGTH            
         B     SPHK22                                                           
         MVI   WORK+1,18                                                        
         MVC   WORK+14(1),TOPRD2   SECOND PRODUCT                               
*                                                                               
         CLI   TOSLN2,0                                                         
         BNE   SPHK20                                                           
         SRA   RE,1                SPLIT THE SECONDS LENGTH                     
         STC   RE,WORK+11          (ASSUMES REQUEST VALIDATION                  
         STC   RE,WORK+15           VALIDATED THE SPLIT LENGTH)                 
         B     SPHK22                                                           
*                                                                               
SPHK20   DS    0H                                                               
         CLI   TOSLN2,0            TEST LEAVE LENGTHS AS THEY ARE               
         BE    SPHK22              YES                                          
         MVC   WORK+11(1),TOSLN1                                                
         MVC   WORK+15(1),TOSLN2                                                
*                                                                               
SPHK22   CLC   WORK+1(1),1(R6)     TEST ELEMENT LENGTH CHANGES                  
         BNE   SPHK24                                                           
         ZIC   RF,1(R6)            NO-JUST MOVE IN THE NEW ELEMENT              
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),WORK                                                     
         B     SPHK26                                                           
*                                                                               
SPHK24   DS    0H                  YES-DELETE THE OLD AND ADD THE NEW           
         GOTO1 RECUP,DMCB,ADBUY,(R6)                                            
*                                                                               
         GOTO1 (RF),(R1),,WORK,(C'R',(R6))                                      
         CLI   8(R1),C'R'                                                       
         BE    SPHK26                                                           
         MVI   SKIPBUY,C'Y'                                                     
         B     SPHKX                                                            
*                                                                               
SPHK26   LA    R0,2                TEST PRD CODE(S) IN BUY ALREADY              
         LA    R8,TOPRD1                                                        
*                                                                               
SPHK28   L     R6,ADBUY                                                         
         LA    R6,24(R6)                                                        
         SR    RE,RE                                                            
*                                                                               
SPHK30   IC    RE,1(R6)                                                         
         AR    R6,RE                                                            
         CLI   0(R6),0                                                          
         BE    SPHK32                                                           
         CLI   0(R6),11                                                         
         BL    SPHK30                                                           
         CLI   0(R6),X'0D'                                                      
         BH    SPHK30                                                           
         C     R6,SPOTADDR                                                      
         BE    SPHK30                                                           
         CLI   1(R6),14                                                         
         BL    SPHK30                                                           
         CLC   10(1,R6),0(R8)      TEST PRD ALREADY ALLOCATED                   
         BE    SPHK38              YES                                          
         CLI   1(R6),18                                                         
         BNE   SPHK30                                                           
         CLC   14(1,R6),0(R8)                                                   
         BE    SPHK38              YES                                          
         B     SPHK30                                                           
*                                                                               
SPHK32   LA    RE,ADDLIST          TEST PRODUCT IN ADDED PRD LIST               
*                                                                               
SPHK34   CLI   0(RE),0                                                          
         BE    SPHK36                                                           
         CLC   0(1,RE),0(R8)                                                    
         BE    SPHK38                                                           
         LA    RE,1(RE)                                                         
         B     SPHK34                                                           
*                                                                               
SPHK36   MVC   0(1,RE),0(R8)       ADD BRAND TO LIST                            
*                                                                               
SPHK38   LA    R8,TOPRD2           SECOND PRODUCT                               
         CLI   0(R8),0                                                          
         BE    SPHK40                                                           
         BCT   R0,SPHK28                                                        
*                                                                               
SPHK40   LM    RE,RF,TOTBUY        ADD SPOTS AND DOLLARS TO BUY TOTALS          
         A     RE,DUB              SPOTS                                        
         A     RF,DUB+4            DOLLARS                                      
         STM   RE,RF,TOTBUY                                                     
         ZIC   R4,0(R3)            AND ALSO FOR THIS BRAND                      
         BCTR  R4,0                                                             
         SLL   R4,3                                                             
         LA    R4,TOTBUY+8(R4)                                                  
         LM    RE,RF,0(R4)                                                      
         A     RE,DUB              SPOTS                                        
         A     RF,DUB+4            DOLLARS                                      
         STM   RE,RF,0(R4)                                                      
*                                                                               
         LA    R3,1(R3)            UPDATE BRDTAB POINTER                        
         CLI   0(R3),0                                                          
         BNE   *+8                                                              
         LA    R3,BRDTAB                                                        
         ST    R3,NEXTBRD                                                       
         B     SPHK46                                                           
*                                                                               
SPHK42   CLC   Q2USER(6),=C'DELETE'   DELETE                                    
         BE    *+12                                                             
         CLI   DELUNMAT,C'Y'          OR DELETE UNMATCHED                       
         BNE   SPHK44                                                           
         MVI   0(R6),X'7F'         SET A MONSTER ELEMENT CODE                   
         B     SPHK46              AND GET OUT                                  
*                                                                               
SPHK44   MVC   WORK(10),0(R6)      UNALLOCATE - MOVE ELEM                       
         MVI   WORK+1,10           SET NEW ELEM LEN                             
         GOTO1 RECUP,DMCB,ADBUY,(R6)                                            
*                                                                               
         GOTO1 (RF),(R1),,WORK,(C'R',(R6))                                      
         CLI   8(R1),C'R'                                                       
         BE    SPHK46                                                           
         MVI   SKIPBUY,C'Y'                                                     
         B     SPHKX                                                            
*                                                                               
SPHK46   CLI   QOPT5,C'Y'          TEST PRODUCT CHANGE REPORT                   
         BNE   SPHKX               NO-DONE                                      
         XC    WREC,WREC           YES-BUILD SORT RECORD                        
         L     R6,ADBUY                                                         
         USING BUYREC,R6                                                        
         MVC   WMED,QMED                                                        
         MVC   WMKT,MKT                                                         
         MVC   WSTA,BIGSTA                                                      
         MVC   WCLT,CLT                                                         
         MVC   WPRD,QPRD                                                        
         MVC   WSTART,QSTART                                                    
         MVC   WEND,QEND                                                        
         MVC   WEST,BUYKEST                                                     
         MVC   WDAY,BDDAY                                                       
         SR    R1,R1               SEQUENCE THE DAYS MON,TUE,WED,...            
         ICM   R1,8,BDDAY                                                       
         SR    RF,RF                                                            
         B     *+14                                                             
         SLL   R1,1                                                             
         LTR   R1,R1                                                            
         BM    *+8                                                              
         BCT   RF,*-10                                                          
         LPR   RF,RF                                                            
         STC   RF,WDAYSEQ                                                       
         MVC   WTIM,BDTIMST                                                     
         MVC   WPRG,BDPROGRM                                                    
         MVC   WLEN,BDSEC                                                       
         L     R1,SPOTADDR                                                      
         MVC   WPRD2,10(R1)        NEW PRODUCT                                  
         MVC   WCOST,BDCOST                                                     
         TM    6(R1),X'20'         TEST COST OVERRIDE                           
         BZ    *+10                                                             
         MVC   WCOST,7(R1)                                                      
         MVC   WDAT,2(R1)          DATE                                         
         GOTO1 DATCON,DMCB,(2,WDAT),DUB     GET MONDAY                          
         GOTO1 GETDAY,(R1),DUB,FULL                                             
         CLC   FULL(3),SPACES                                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         ZIC   RF,0(R1)                                                         
         BCTR  RF,0                                                             
         LNR   RF,RF                                                            
         BZ    SPHK48                                                           
         ST    RF,DMCB+8                                                        
         GOTO1 ADDAY,DMCB,DUB,WORK                                              
         GOTO1 DATCON,(R1),WORK,(2,WDAT)                                        
*                                                                               
SPHK48   MVC   WMKTNM,MKTNM        NAMES                                        
         MVC   WCLTNM,CLTNM                                                     
         MVC   WPRDNM,PRDNM                                                     
         MVC   WSYSNAME,SVSYSNM                                                 
         L     R1,ADBUY                                                         
         ZIC   RF,BUYKEST-BUYKEY(R1)                                            
         BCTR  RF,0                                                             
         MH    RF,=Y(L'ESTNAMES)                                                
         LA    RF,ESTNAMES(RF)                                                  
         MVC   WESTNM,0(RF)                                                     
         CLI   RCSUBPRG,0          TEST UNALL REQUEST                           
         BNE   SPHK50                                                           
         MVI   WPRD2,0                                                          
         CLC   QAREA+49(6),=C'DELETE'   YES-PRD2=0 FOR DELETE                   
         BE    SPHK52                                                           
         CLI   DELUNMAT,C'Y'                                                    
         BE    SPHK52                                                           
         MVI   WPRD2,X'FF'                  PRD2=X'FF' FOR UNALL                
         B     SPHK52                                                           
         DROP  R6                                                               
*                                                                               
SPHK50   ZIC   RE,WPRD2            GET NAME OF NEW PRODUCT                      
         BCTR  RE,0                                                             
         MH    RE,PRDBUFLN                                                      
         L     RF,PRDBUFF                                                       
         LA    RE,0(RE,RF)                                                      
         MVC   WPRDCD2,1(RE)                                                    
         MVC   WPRDNM2,4(RE)                                                    
*                                                                               
SPHK52   CLI   SORTOPEN,C'Y'       TEST SORT OPEN YET                           
         BE    SPHK54                                                           
         MVI   SORTOPEN,C'Y'       NO-OPEN IT NOW                               
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD                                 
*                                                                               
SPHK54   GOTO1 =V(SORTER),DMCB,=C'PUT',WREC                                     
*                                                                               
SPHKX    MVI   SPOTYORN,C'N'                                                    
         B     EXIT                                                             
*                                                                               
HKDMCB   DS    6F                                                               
         EJECT                                                                  
* ERROR HAS OCCURED - BRAND ESTIMATE NOT OPEN                                   
*                                                                               
SPHKERR  LA    R4,P                                                             
         USING UNLINED,R4                                                       
         ZIC   R1,0(R3)                                                         
         BCTR  R1,0                                                             
         MH    R1,=Y(UNPRD3-UNPRD2)                                             
         LA    R1,UNPRD2(R1)                                                    
         MVC   0(16,R1),=CL16'**EST NOT OPEN**'                                 
*                                                                               
*                                                                               
         LA    R3,1(R3)            UPDATE BRDTAB POINTER                        
         CLI   0(R3),0                                                          
         BNE   *+8                                                              
         LA    R3,BRDTAB                                                        
         ST    R3,NEXTBRD                                                       
         MVI   REPSW,C'N'          ENSURE DON'T ADD PRD(S) TO BDMASPRD          
         BAS   RE,UNPRTLN          ...BUT THAT MEANS WE MUST                    
         GOTO1 REPORT              ...PRINT IT NOW OR ELSE WON'T PRINT          
         MVI   MKTSW,C'Y'                                                       
         MVI   STASW,C'Y'                                                       
         B     SPHKX                                                            
*                                                                               
*&&DO                                                                           
         LA    R3,1(R3)            POINT TO NEXT PRD                            
         CLI   0(R3),0             TEST E-O-L                                   
         BNE   SPHK10               NO - TRY NEXT BRAND                         
         CLI   ERRFLAG,C'Y'        TEST REACHED E-O-L BEFORE                    
         BE    SPHKX                YES -FORGET IT                              
         MVI   ERRFLAG,C'Y'        ELSE SET FLAG                                
         LA    R3,BRDTAB           POINT TO START                               
         B     SPHK10               AND TRY AGAIN                               
*&&                                                                             
         SPACE 2                                                                
* ERROR HAS OCCURED - AAA-AAA SPOT SITUATION OCCURED                            
*                                                                               
SPHKERR2 LA    R3,1(R3)            GO ON TO NEXT ALLOCATION                     
         CLI   0(R3),0                                                          
         BNE   SPHK10                                                           
         CLI   ERRFLAG2,C'Y'                                                    
         BE    *+16                                                             
         MVI   ERRFLAG2,C'Y'                                                    
         LA    R3,BRDTAB                                                        
         B     SPHK10                                                           
         BAS   RE,UNPRTLN          NO ALLOCATION WORKS -                        
         USING UNLINED,R4          PRINT WARNING THAT SPOT WAS SKIPPED          
         LA    RE,UNPRD1                                                        
         MVC   0(17,RE),=C'*** WARNING: SPOT'                                   
         LA    RF,Q2USER                                                        
         LA    R1,QPRD                                                          
         CLC   BPRD,10(R6)                                                      
         BE    *+10                                                             
         LR    R1,RF                                                            
         LA    RF,QPRD                                                          
         MVC   18(3,RE),0(R1)                                                   
         MVI   21(RE),C'-'                                                      
         MVC   22(3,RE),0(RF)                                                   
         MVC   26(20,RE),=C'IS BEING SKIPPED ***'                               
         GOTO1 REPORT                                                           
         MVI   MKTSW,C'Y'                                                       
         MVI   STASW,C'Y'                                                       
         B     SPHKX                                                            
         EJECT                                                                  
* STALAST                                                                       
*                                                                               
UN200    LA    R4,P                                                             
         USING UNLINED,R4                                                       
         OC    TOTSTA,TOTSTA                                                    
         BZ    EXIT                                                             
         LA    R1,TOTSTA                                                        
         BAS   RE,UNFMT                                                         
         MVC   UNSTA(14),=CL14'STATION TOTALS'                                  
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         SPACE 2                                                                
* MKTLAST                                                                       
*                                                                               
UN210    LA    R4,P                                                             
         USING UNLINED,R4                                                       
         OC    TOTMKT,TOTMKT                                                    
         BZ    EXIT                                                             
         LA    R1,TOTMKT                                                        
         BAS   RE,UNFMT                                                         
         MVC   UNSTA(14),=CL14'MARKET  TOTALS'                                  
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         SPACE 2                                                                
* EST LAST (PRODUCT TOTALS)                                                     
*                                                                               
UN220    LA    R4,P                                                             
         USING UNLINED,R4                                                       
         OC    TOTEST,TOTEST                                                    
         BZ    EXIT                                                             
         GOTO1 REPORT              SKIP A LINE                                  
         LA    R1,TOTEST                                                        
         BAS   RE,UNFMT                                                         
         MVC   UNSTA(14),=CL14'PRODUCT TOTALS'                                  
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         SPACE 2                                                                
* CLT LAST                                                                      
*                                                                               
UN230    LA    R4,P                                                             
         USING UNLINED,R4                                                       
         OC    TOTCLT,TOTCLT                                                    
         BZ    EXIT                                                             
         GOTO1 REPORT              SKIP A LINE                                  
         LA    R1,TOTCLT                                                        
         BAS   RE,UNFMT                                                         
         MVC   UNMKT(14),=CL14'CLIENT  TOTALS'                                  
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         EJECT                                                                  
* RUNLAST                                                                       
UN240    TM    RUNFLAG,RUNFLOCK    ISSUE UNLOCK IF UPDATIVE SOON RUN            
         BZ    UN241                                                            
         GOTOR =V(LOCKET),DMCB,('LKUNLKQ',LKKEYD),ACOMFACS                      
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
* PRINT PRODUCT CHANGE REPORT IF REQUESTED                                      
*                                                                               
UN241    CLI   SORTOPEN,C'Y'                                                    
         BNE   EXIT                                                             
         MVI   MODE,STALAST                                                     
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,6                                                       
         MVC   QEST,=C'ALL'                                                     
         MVC   QESTEND,SPACES                                                   
         MVI   SPSUPMKT,C'N'                                                    
         XC    WKEYSV,WKEYSV                                                    
*                                                                               
UN242    DS    0H                  GET SORT RECORDS                             
         GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   RF,15,4(R1)                                                      
         BNZ   *+12                                                             
         MVI   SORTEND,C'Y'                                                     
         B     UN243                                                            
         MVC   WREC,0(RF)                                                       
         LA    R4,P                                                             
         USING SPLINED,R4                                                       
         CLC   WKEY,WKEYSV         TEST KEY CHANGE                              
         BNE   *+12                                                             
         LA    R2,1(R2)            NO-AUGMENT N'SPOTS                           
         B     UN242                  AND GET NEXT RECORD                       
         OC    WKEYSV,WKEYSV       YES-TEST FIRST TIME                          
         BZ    UN244                                                            
*                                                                               
UN243    EDIT  (R2),(4,SPSPT)      NO-EDIT N'SPOTS                              
         CLC   WKEY(WPRD2-WKEY),WKEYSV                                          
         BE    *+8                                                              
         MVI   SPACING,2              SPACE AFTER IF DATE CHANGE                
         GOTO1 REPORT                 AND PRINT A LINE                          
         CLI   SORTEND,C'Y'                                                     
         BE    UN260                                                            
*                                                                               
UN244    LA    R2,1                RESET SPOT COUNTER                           
         CLC   WMED,MED            TEST MEDIA CHANGE                            
         BE    UN246                                                            
         MVC   MED,WMED            YES-GET MEDIA NAME                           
         GOTO1 MEDGET,DMCB,(MED,AGY),DATAMGR,WORK                               
         CLI   8(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   BAGYMD,WORK                                                      
         MVC   MEDNM,WORK+1                                                     
*                                                                               
UN246    MVC   MKT,WMKT            SET MARKET                                   
         MVC   MKTNM,WMKTNM                                                     
         MVC   BIGSTA(8),WSTA      SET STATION                                  
         MVC   SVSYSNM,WSYSNAME                                                 
         MVC   CLT,WCLT            SET CLIENT AND PRODUCT                       
         MVC   CLTNM,SPACES                                                     
         MVC   CLTNM(L'WCLTNM),WCLTNM                                           
         MVC   PRODUCT,WPRD                                                     
         MVC   PRDNM,SPACES                                                     
         MVC   PRDNM(L'WPRDNM),WPRDNM                                           
         MVC   QSTART,WSTART       SET REQUEST DATES                            
         MVC   QEND,WEND                                                        
*                                                                               
         CLC   WKEY(WCLT-WKEY),WKEYSV   TEST NEW STATION                        
         BE    UN247                                                            
         MVI   FORCEHED,C'Y'            YES-FORCE BLANK PAGE                    
         MVI   P,0                                                              
         MVI   RCSUBPRG,7                                                       
         GOTO1 REPORT                                                           
         MVI   RCSUBPRG,6                                                       
         MVC   PAGE,=H'1'               AND START NEW REPORT                    
*                                                                               
UN247    CLC   WKEY(WEST-WKEY),WKEYSV   TEST NEW PERIOD                         
         BE    *+8                                                              
         MVI   FORCEHED,C'Y'            YES-FORCE PAGE BREAK                    
*                                                                               
         MVI   NEWEST,C'N'                                                      
         CLC   WKEY(WDAYSEQ-WKEY),WKEYSV  TEST NEW ESTIMATE                     
         BE    *+8                                                              
         MVI   NEWEST,C'Y'         YES                                          
         MVI   HEADHK,C'N'                                                      
         BAS   RE,SPFMT            FORMAT BUYLINE DETAILS                       
         MVI   TRADEBUY,C'N'                                                    
         CLC   AGY,=C'DF'          TEST SAATCHI                                 
         BNE   UN248                                                            
         LA    R1,WPRG             AND IT'S A TRADE BUY                         
         LA    R0,L'WPRG-1                                                      
         CLC   0(2,R1),=C'-T'                                                   
         BE    *+16                                                             
         LA    R1,1(R1)                                                         
         BCT   R0,*-14                                                          
         B     UN248                                                            
         MVI   TRADEBUY,C'Y'       YES                                          
*                                                                               
UN248    CLI   WPRD2,0             NEW PRODUCT                                  
         BNE   *+14                                                             
         MVC   SPPRD(7),=C'DELETED'                                             
         B     UN250                                                            
         CLI   WPRD2,X'FF'                                                      
         BNE   *+14                                                             
         MVC   SPPRD(5),=C'UNALL'                                               
         B     UN250                                                            
         MVC   SPPRD,WPRDCD2                                                    
         MVC   SPPRDNM,WPRDNM2                                                  
*                                                                               
UN250    CLI   TRADEBUY,C'Y'       FORMAT THE COST                              
         BNE   *+14                                                             
         MVC   SPCOST(5),=C'TRADE'                                              
         B     UN252                                                            
         EDIT  (B3,WCOST),(10,SPCOST),2,ALIGN=LEFT,FLOAT=$                      
*                                                                               
UN252    MVC   WKEYSV,WKEY         SAVE THE KEY                                 
         B     UN242               GET NEXT RECORD                              
*                                                                               
UN260    MVI   MODE,RUNLAST                                                     
         B     EXIT                                                             
         EJECT                                                                  
NEXTEL   CLI   0(R6),0                                                          
         BE    NEXTELX                                                          
         SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
*                                                                               
NEXTEL2  CLI   0(R6),0                                                          
         BE    NEXTELX                                                          
         CLC   ELCDLO,0(R6)                                                     
         BH    NEXTEL                                                           
         CLC   ELCDHI,0(R6)                                                     
         BL    NEXTEL                                                           
         CR    RE,RE               EXIT WITH CC EQ                              
         BR    RE                                                               
*                                                                               
NEXTELX  LTR   RE,RE               EXIT WITH CC NEQ                             
         BR    RE                                                               
*                                                                               
ELCDLO   DC    X'00'                                                            
ELCDHI   DC    X'00'                                                            
*                                                                               
* PRINT BUYLINE DETAILS                                                         
* R4=A(PRINT LINE)                                                              
*                                                                               
UNPRTLN  LR    R0,RE                                                            
         LA    R4,P                                                             
         USING UNLINED,R4                                                       
         CLI   MKTSW,C'Y'          TEST MKT NAME PRINTED                        
         BE    UNPRTLN2                                                         
         MVC   P2,P                MOVE ERROR MESSAGES IF ANY                   
         MVC   P,SPACES                                                         
         MVC   UNMKT,MKT                                                        
         MVC   UNMKT+6(24),MKTNM                                                
         LA    R4,P2                                                            
*                                                                               
UNPRTLN2 CLI   STASW,C'Y'          TEST STATION PRINTED                         
         BE    *+14                                                             
         MVI   ALLOWLIN,6                                                       
         MVC   UNSTA,BIGSTA                                                     
         L     RE,ADBUY                                                         
         USING BUYREC,RE                                                        
         ZIC   RF,9(RE)                                                         
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  UNEST,DUB                                                        
         MVI   UNEST+3,C'-'                                                     
*                                                                               
         LLC   RF,BUYKBUY                                                       
         TM    BUYRCNTL,BUYRLN2    2-BYTE LINE NUMBERS?                         
         BZ    *+8                                                              
         ICM   RF,3,BUYRLIN                                                     
*                                                                               
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  UNLIN,DUB                                                        
         DROP  RE                                                               
*                                                                               
         CLI   HASCUTIN,C'Y'                                                    
         BNE   *+10                                                             
         MVC   UNCUTIN,=CL2'/C'                                                 
*                                                                               
UNPRTLNX LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
* R4 HAS PRINT LINE ADDRESS                                                     
* R1 HAS ACCUM ADDRESS                                                          
*                                                                               
UNFMT    NTR1                                                                   
         USING UNLINED,R4                                                       
         LA    RF,MAXBRDS+1                                                     
         LA    R3,UNPRD1                                                        
UNFMT2   OC    0(8,R1),0(R1)                                                    
         BZ    UNFMT4                                                           
         L     R0,0(R1)                                                         
         EDIT  (R0),(5,(R3))       SPOTS                                        
         L     R0,4(R1)                                                         
         EDIT  (R0),(10,6(R3)),2   DOLLARS                                      
*                                                                               
UNFMT4   LA    R3,17(R3)                                                        
         LA    R1,8(R1)                                                         
         BCT   RF,UNFMT2                                                        
         B     EXIT                                                             
         SPACE 2                                                                
* FORMAT BUYLINE DETAILS FOR PRODUCT CHANGE REPORT                              
* R4=A(PRINT LINE)                                                              
*                                                                               
SPFMT    NTR1  ,                                                                
         USING SPLINED,R4                                                       
         CLI   HEADHK,C'Y'                                                      
         BE    *+12                                                             
         CLI   NEWEST,C'Y'                                                      
         BNE   SPFMT2                                                           
         ZIC   RE,WEST                                                          
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SPEST,DUB                                                        
         MVC   SPESTNM,WESTNM                                                   
*                                                                               
SPFMT2   CLI   HEADHK,C'Y'                                                      
         BE    *+14                                                             
         CLC   WKEY(WDAT-WKEY),WKEYSV                                           
         BE    SPFMT4                                                           
         GOTO1 CODAY,DMCB,WDAY,SPDAY     (REALLY DAYUNPK)                       
         GOTO1 UNTIME,DMCB,WTIM,SPTIM                                           
         MVC   SPPRG,WPRG                                                       
         EDIT  WLEN,(3,SPLEN)                                                   
*                                                                               
SPFMT4   CLI   HEADHK,C'Y'                                                      
         BE    *+14                                                             
         CLC   WKEY(WPRD2-WKEY),WKEYSV                                          
         BE    SPFMTX                                                           
         GOTO1 DATCON,DMCB,(2,WDAT),(4,SPDAT)                                   
*                                                                               
SPFMTX   B     EXIT                                                             
         EJECT                                                                  
SPK102   CSECT                                                                  
         CNOP  0,4                                                              
         USING *,RF                                                             
UNHDHK   NTR1                                                                   
         LM    R7,RC,HDHKR7                                                     
         B     HDHK2                                                            
HDHKR7   DC    6F'0'                                                            
         DROP  RF                                                               
*                                                                               
HDHK2    CLI   RCSUBPRG,5          TEST PRODUCT CHANGE REPORT                   
         BH    HDHK10              YES                                          
         CLI   MKTSW,C'Y'          NO-TEST MKT NAME PRINTED YET                 
         BNE   HDHK4               NO                                           
         CLI   MODE,MKTLAST                                                     
         BH    HDHK4                                                            
         MVC   MID1(24),MKTNM                                                   
         MVC   MID1+25(11),=C'(CONTINUED)'                                      
         CLI   MODE,MKTLAST                                                     
         BE    HDHK4                                                            
         LA    R4,P                                                             
         USING UNLINED,R4                                                       
         MVC   UNSTA,BIGSTA                                                     
*                                                                               
HDHK4    CLI   RCSUBPRG,0          TEST UNALL REQUEST                           
         BNE   HDHK6                                                            
         CLC   =C'DELETE',QAREA+49                                              
         BNE   HDHKX                                                            
         MVC   H8+26(11),=C'--DELETED--'                                        
         B     HDHKX                                                            
*                                                                               
HDHK6    ZIC   RF,RCSUBPRG                                                      
         LA    R5,Q2USER                                                        
         LA    R6,H8+41                                                         
*                                                                               
HDHK8    MVC   0(9,R6),=C'--PRODUCT'                                            
         MVC   10(3,R6),0(R5)                                                   
         MVC   14(2,R6),=C'--'                                                  
         CLC   3(3,R5),SPACES                                                   
         BNH   HDHK9                                                            
         MVC   1(11,R6),2(R6)                                                   
         MVI   12(R6),C'-'                                                      
         MVC   13(3,R6),3(R5)                                                   
         CLI   11(R6),C' '                                                      
         BH    HDHK9                                                            
         MVC   11(4,R6),12(R6)                                                  
         MVI   15(R6),C' '                                                      
         CLI   14(R6),C' '                                                      
         BH    HDHK9                                                            
         MVI   15(R6),C'-'                                                      
*                                                                               
HDHK9    LA    R5,13(R5)                                                        
         LA    R6,17(R6)                                                        
         BCT   RF,HDHK8                                                         
         B     HDHKX                                                            
*                                  PRODUCT CHANGE REPORT                        
HDHK10   CLI   P,0                 TEST BLANK SEPERATOR PAGE                    
         BE    HDHKX               YES                                          
         MVC   H6+38(6),=C'MARKET'                                              
         MVC   H6+46(4),MKT                                                     
         MVC   H6+53(24),MKTNM                                                  
         MVC   H7+38(7),=C'STATION'                                             
         MVC   H7+46(8),BIGSTA                                                  
         CLC   SVSYSNM,SPACES      CABLE SYSTEM NAME (IF ANY)                   
         BNH   *+10                                                             
         MVC   H7+55(L'SVSYSNM),SVSYSNM                                         
         CLI   NEWEST,C'Y'         TEST FIRST LINE FOR AN ESTIMATE              
         BE    HDHKX                                                            
         MVC   P2,P                NO-CONTINUE PREVIOUS ESTIMATE                
         MVC   P,SPACES                                                         
         MVC   P(11),=C'(CONTINUED)'                                            
         MVI   HEADHK,C'Y'                                                      
         LA    R4,P2                                                            
         BAS   RE,SPFMT                                                         
*                                                                               
HDHKX    B     EXIT                                                             
*                                                                               
         DS    0D                                                               
TOTBUY   DS    XL((MAXBRDS+1)*8)                                                
TOTSTA   DS    XL((MAXBRDS+1)*8)                                                
TOTMKT   DS    XL((MAXBRDS+1)*8)                                                
TOTEST   DS    XL((MAXBRDS+1)*8)                                                
TOTCLT   DS    XL((MAXBRDS+1)*8)                                                
*                                                                               
BRDTAB   DS    XL128               BRAND SELECTION LIST                         
ADDLIST  DS    XL128               LIST OF ADDED PRD CODES FOR DATAMGR          
*                                                                               
RELO     DS    A                                                                
NEXTBRD  DS    A                   A(NEXT BRDTAB ENTRY)                         
*                                                                               
COSTAREA DS    8F                                                               
*                                                                               
ERRFLAG  DC    X'00'                                                            
ERRFLAG2 DC    X'00'                                                            
BRDLIST  DS    XL(MAXBRDS*TOPRDL+1)      REALLOCATION LIST                      
MAXBRDS  EQU   5                                                                
*                                                                               
*                                                                               
SVQPRD   DS    CL3                                                              
SVSYSNM  DS    CL(L'SSYSNAME)                                                   
WKEYSV   DS    CL(L'WKEY)                                                       
NEWEST   DS    CL1                                                              
HEADHK   DS    CL1                                                              
TRADEBUY DS    CL1                                                              
LASTBRD  DS    CL1                                                              
FRSTPRD1 DS    XL1                                                              
FRSTPRD2 DS    XL1                                                              
DELUNMAT DS    CL1                                                              
DELSPOTS DS    CL1                                                              
*                                                                               
QSLN1    DS    XL1                                                              
QSLN2    DS    XL1                                                              
QSLNT    DS    XL1                                                              
*                                                                               
WREC     DS    0CL194              SORT RECORD                                  
WKEY     DS    0CL63                                                            
WMED     DS    CL1                                                              
WMKT     DS    CL4                                                              
WSTA     DS    CL8                                                              
WCLT     DS    CL3                                                              
WPRD     DS    CL3                                                              
WSTART   DS    CL6                                                              
WEND     DS    CL6                                                              
WEST     DS    XL1                                                              
WDAYSEQ  DS    XL1                                                              
WDAY     DS    XL1                                                              
WTIM     DS    XL4                                                              
WPRG     DS    CL18                                                             
WLEN     DS    XL1                                                              
WDAT     DS    XL2                                                              
WPRD2    DS    XL1                                                              
WCOST    DS    XL3                                                              
*                                                                               
WMKTNM   DS    CL24                                                             
WCLTNM   DS    CL20                                                             
WPRDNM   DS    CL20                                                             
WPRDNM2  DS    CL20                                                             
WPRDCD2  DS    CL3                                                              
WESTNM   DS    CL20                                                             
WSYSNAME DS    CL24                                                             
*                                                                               
MYKEY    DS    CL13                                                             
LOCKKEYW DS    CL(L'LOCKEY)        LOCKET KEY FOR UPDATIVE SOON RUN             
RUNFLAG  DS    X                   ** RUN FLAG **                               
RUNFSOON EQU   X'80'               RUNNING SOON REQUEST                         
RUNFLOCK EQU   X'40'               ISSUE UNLOCK AT REQLAST                      
HASCUTIN DS    X                   CUT-IN FOUND INDICATOR                       
*                                                                               
ESTERRFL DS    XL1                                                              
*                                                                               
SORTOPEN DS    CL1                                                              
SORTEND  DS    CL1                                                              
*                                                                               
SKIPBUY  DS    C                                                                
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,63,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=194'                                   
         SPACE 2                                                                
         LTORG                                                                  
*                                                                               
         DS    0D                                                               
ESTNAMES DS    255CL20             ESTIMATE NAME TABLE                          
*                                                                               
         DS    0D                                                               
         DC    CL8'*ESTTAB*'                                                    
ESTTAB   DS    1024C               TABLE FOR OPEN EST CHECKING                  
         EJECT                                                                  
*                                                                               
*****************************************************************               
* CODE FOR CANADIAN STATIONS                                                    
*****************************************************************               
*                                                                               
XPLODE   NTR1  BASE=*,LABEL=*                                                   
         L     RE,=A(MYIO)                                                      
         LHI   RF,6200                                                          
         XCEFL                                                                  
*                                                                               
         L     R0,=A(MYIO)          COPY NETW BUY IN MY BUFFER                  
         L     RE,ADBUY                                                         
         SR    R1,R1                                                            
         ICM   R1,3,13(RE)                                                      
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         BAS   RE,BLDPRDS           BUILD TAB OF ALLOC PRODUCTS                 
         BNE   XPEXIT               IF NO ALLOC PROD, EXIT                      
*                                                                               
         L     R6,=A(MYIO)          LOOK FOR X'68'ELEMS IN NETW BUY             
         USING BUYREC,R6                                                        
         MVC   NPRD1(2),BDMASPRD    SAVE NETW MASPRD TO USE IN LOCAL            
         LA    R6,BDELEM                                                        
XPLO10   CLI   0(R6),0                                                          
         BE    XPEXIT                                                           
         CLI   0(R6),X'68'                                                      
         BE    XPLO20                                                           
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     XPLO10                                                           
*                                                                               
XPLO20   ST    R6,X68PTR           REMEBER WHERE STOPED                         
         LR    R5,R6                                                            
         USING NTWKELEM,R5         BUILD KEY FOR LOCAL STAT                     
         XC    KEY,KEY                                                          
         MVC   KEY(13),KEY1                                                     
         MVC   KEY+4(5),NTWKMKST                                                
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   XPLO30                                                           
         MVC   AREC,ADBUY                                                       
         GOTO1 GET                  GET LOC STAT BUY                            
         L     R6,ADBUY                                                         
         MVC   BDMASPRD(1),NPRD1    SET MASPRD FOR LOCAL                        
         MVC   BDMASPRD+1(1),NPRD2                                              
         BAS   RE,CPYPRDS           COPY 0B ELEMS FROM NETW BUY                 
         LA    R1,ADDLIST                                                       
         ST    R1,DM6                                                           
         MVC   AREC,ADBUY           PUT MODIFIED REC BACK                       
         GOTO1 PUT                  STORE MODIFIED REC                          
         CLC   =C'TRACE ',QUESTOR                                               
         BNE   *+8                                                              
         BAS   RE,UNTRCPY                                                       
XPLO30   L     R6,X68PTR            RETURN TO NETW BUY WHERE LEFT OF            
         ZIC   R0,1(R6)             LOOK FOR ANOTHER 68 ELEM                    
         AR    R6,R0                                                            
         B     XPLO10                                                           
XPEXIT   DS    0H                                                               
         L     R0,ADBUY            PUT NETW BUY BACK IN ADBUY                   
         L     RE,=A(MYIO)                                                      
         SR    R1,R1                                                            
         ICM   R1,3,13(RE)                                                      
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         DROP  R5,R6                                                            
         XIT1                                                                   
**                                                                              
******* BLDPRDS SUBROUTINE ***********                                          
**                                                                              
BLDPRDS  NTR1                       BIULD THE TAB OF PRODS TO COPY              
         MVI   PRDNUM,0                                                         
         MVI   SEQNUM,0                                                         
         LA    R2,PRDTAB                                                        
*                                                                               
         USING BUYREC,R6                                                        
         L     R6,ADBUY                                                         
         LA    R6,BDELEM                                                        
*                                                                               
         USING REGELEM,R6                                                       
BLDPR10  CLI   0(R6),0                                                          
         BE    BLDPR50                                                          
         CLI   0(R6),X'0B'          IF 0B ELEM FOUND                            
         BE    BLDPR18                                                          
*                                                                               
BLDPR15  CLI   0(R6),X'0C'                                                      
         BNE   BLDPR40                                                          
         TM    RSTATUS,X'80'                                                    
         BO    BLDPR40                                                          
*                                                                               
BLDPR18  DS    0H                                                               
         ZIC   R1,SEQNUM            ASSIGN IT A SEQUENCE NUMB                   
         LA    R1,1(R1)                                                         
         STC   R1,SEQNUM            AND STORE IT IN MYTAB                       
*                                                                               
         CLC   RDATE,BQSTARTP                                                   
         BL    BLDPR40                                                          
         CLC   RDATE,BQENDP                                                     
         BH    BLDPR40                                                          
*                                                                               
         XC    0(19,R2),0(R2)       STORE ELEM IN MYTAB WITH SEQ#               
         MVC   0(1,R2),SEQNUM                                                   
         ZIC   R1,1(R6)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R2),0(R6)                                                    
         LA    R2,19(R2)                                                        
         ZIC   R1,PRDNUM                                                        
         LA    R1,1(R1)                                                         
         STC   R1,PRDNUM                                                        
*                                                                               
         CHI   R1,PRDTABNQ         MAX ENTRIES NOW 200                          
         BL    *+6                                                              
         DC    H'0'                NEED TO MAKE PRDTAB BIGGER                   
*                                                                               
BLDPR40  ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     BLDPR10                                                          
BLDPR50  MVI   0(R2),X'FF'          MARK EOT                                    
         OC    PRDNUM,PRDNUM        CK IF ANY TAB ENTRIES MADE                  
         BNZ   GOOD                                                             
         CR    RB,R9                IF NOT, SET CC NOT=                         
         B     XITPRD                                                           
GOOD     CR    RB,RB                IF YES, CC =                                
XITPRD   XIT1                                                                   
**                                                                              
*********** COPY PRODUCTS SUB *********                                         
**                                                                              
CPYPRDS  NTR1                       COPY ALLOC NETW ELEMS INTO LOC ELEM         
         USING BUYREC,R6                                                        
         LA    R2,PRDTAB                                                        
CPYPR10  CLI   0(R2),X'FF'                                                      
         BE    CPYPR70                                                          
         L     R6,ADBUY                                                         
         LA    R6,BDELEM                                                        
         MVI   SEQNUM,0                                                         
*                                                                               
         USING REGELEM,R6                                                       
CPYPR30  CLI   0(R6),0                                                          
         BE    CPYPR50                                                          
         CLI   0(R6),X'0B'                                                      
         BE    CPYPR34                                                          
*                                                                               
CPYPR32  CLI   0(R6),X'0C'                                                      
         BNE   CPYPR40                                                          
         TM    RSTATUS,X'80'                                                    
         BO    CPYPR40                                                          
*                                                                               
CPYPR34  DS    0H                                                               
         ZIC   R1,SEQNUM                                                        
         LA    R1,1(R1)                                                         
         STC   R1,SEQNUM                                                        
*                                                                               
         CLC   SEQNUM,0(R2)                                                     
         BNE   CPYPR40                                                          
         CLC   RDATE,BQSTARTP                                                   
         BL    CPYPR40                                                          
         CLC   RDATE,BQENDP                                                     
         BH    CPYPR40                                                          
*                                                                               
         XC    WORK,WORK            PUT NEW ELEM IN WORK                        
         MVC   WORK(10),0(R6)                                                   
         MVC   WORK+10(8),11(R2)                                                
         MVC   WORK+1(1),2(R2)                                                  
         CLI   QOPT2,C'P'           PRESERVING CUTINS?                          
         BNE   CPYPR39              -NOPE                                       
*                                                                               
*** CODE TO PRESERVE CUT-INS                                                    
*   NOTE: WE DON'T CHECK IF BILLED/AFFID AS THESE ONLY SET @NWK LEVEL!          
         CLC   RLEN,WORK+1         TEST SAME #BRDS ALLOCATED                    
         BNE   CPYPR38             -NOPE, MUST BE A CUTIN                       
         SR    RF,RF                                                            
         IC    RF,RLEN                                                          
         SHI   RF,1                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   WORK(0),0(R6)       TEST ACTUALLY CHANGING DETAILS               
         BE    CPYPR39             -NOPE, NO ACTIVITY FOR THIS SPOT             
*                                                                               
         CLI   RLEN,RLPOL1LQ       TEST SINGLE BRAND ALLOCATED                  
         BNE   CPYPR36                                                          
         CLI   BPRD2,0             YES-MUST BE CUT-IN IF P/B REQUEST            
         BNE   CPYPR38                                                          
         CLC   BPRD,RPPRD          RIGHT BRAND                                  
         BNE   CPYPR38             -NOPE, IS A CUTIN                            
         B     CPYPR39                                                          
*                                                                               
CPYPR36  CLI   RLEN,RLPOL2LQ       TEST PIGGYBACK                               
         BNE   CPYPR39             NO-WHY ARE WE HERE? CHANGE IT ANYWAY         
         CLI   BPRD2,0             TEST PIGGYBACK REQUESTED                     
         BNE   CPYPR37             -YES                                         
         CLI   QOPT4,C'Y'          TEST TO INCLUDE PIGGYBACKS                   
         BNE   CPYPR38             -NO, MUST BE A CUTIN                         
* FOLLOWING WILL PASS A CUTIN PB IF EITHER PRD IS THE ONE BEING CHANGED         
* THE PB PAIR ITSELF MAY NOT MATCH THAT OF THE NWK BUT IS CHANGED SINCE         
* FITS CHANGE CRITERIA OF 1 PRD MATCHING.                                       
         CLC   BPRD,RPPRD          ONE OF THE PRODUCTS MUST MATCH               
         BE    CPYPR39                                                          
         CLC   BPRD,RPALLOC2+(RPPRD-RPALLOC)                                    
         BE    CPYPR39                                                          
         B     CPYPR38             NEITHER MATCH SO PRESERVE CUT-IN             
*                                                                               
CPYPR37  CLC   BPRD,RPPRD          EACH PRODUCT MUST MATCH                      
         BNE   CPYPR38                                                          
         CLC   BPRD2,RPALLOC2+(RPPRD-RPALLOC)                                   
         BNE   CPYPR38                                                          
         CLI   QSLN2,0             TEST 2ND LENGTH FILTER                       
         BE    CPYPR39                                                          
         CLC   QSLN1,RPTIME        YES-1ST AND 2ND LENGTHS MUST MATCH           
         BNE   CPYPR38                                                          
         CLC   QSLN2,RPALLOC2+(RPTIME-RPALLOC)                                  
         BE    CPYPR39                                                          
*                                                                               
CPYPR38  MVI   HASCUTIN,C'Y'                                                    
         B     CPYPR50             DO NOT CHANGE CUT-IN                         
*** END OF CODE TO PRESERVE CUT-INS                                             
*                                                                               
CPYPR39  GOTO1 RECUP,DMCB,(C'S',ADBUY),(R6),0                                   
         GOTO1 RECUP,DMCB,(C'S',ADBUY),WORK,(R6)                                
         B     CPYPR50                                                          
*                                                                               
CPYPR40  ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     CPYPR30                                                          
*                                                                               
CPYPR50  LA    R2,19(R2)                                                        
         B     CPYPR10                                                          
*                                                                               
CPYPR70  XIT1                                                                   
**************                                                                  
UNTRCPY  NTR1                                                                   
*                                                                               
         GOTO1 REPORT              SKIP A LINE                                  
         L     R6,ADBUY                                                         
         SR    R0,R0                                                            
         ICM   R0,3,13(R6)                                                      
*                                                                               
         GOTO1 =V(PRTREC),DMCB,(C'E',ADBUY),(24,13),V(PRINT),          X        
               V(HEXOUT)                                                        
*                                                                               
*        GOTO1 PRNTBL,TRCDMCB,=C'BUYREC',ADBUY,C'DUMP',                         
*              (R0),=C'1D',RR=RELO                                              
         GOTO1 REPORT              SKIP A LINE                                  
         GOTO1 PRNTBL,TRCDMCB,=C'ADDED PRD LIST',ADDLIST,C'DUMP',      X        
               128,=C'1D',RR=RELO                                               
         GOTO1 REPORT              SKIP A LINE                                  
         XIT1                                                                   
**********************************************                                  
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
PRDNUM   DS    X                                                                
SEQNUM   DS    X                                                                
NPRD1    DS    X                                                                
NPRD2    DS    X                                                                
X68PTR   DS    F                                                                
TRCDMCB  DS    6F                                                               
*                                                                               
PRDTABNQ EQU   300                                                              
PRDTAB   DS    (PRDTABNQ*19)X      ROOM FOR 300 19BYTE ENTRIES                  
PRDTABX  DC    X'FF'                                                            
*                                                                               
MYIO     DS    6200X                                                            
*****************************************************************               
TOPRDD   DSECT                     REALLOCATION DSECT                           
*                                                                               
TOPRD1   DS    XL1                                                              
TOPRD2   DS    XL1                                                              
TOSLN1   DS    XL1                                                              
TOSLN2   DS    XL1                                                              
TOWGT    DS    XL1                                                              
*                                                                               
TOPRDL   EQU   *-TOPRDD                                                         
         EJECT                                                                  
******************** S P E S T C H K ****************************               
*                                                               *               
* SUBROUTINE MAINTAINS TABLE OF ESTIMATE NUMBERS OPEN BY BRAND  *               
* FIRST 32 BYTES ARE LIST OF ESTIMATE NUMBERS IN TABLE          *               
* FOLLOWED BY 4  BYTE ENTRY FOR EACH BRAND (MAX 220)            *               
* IF MORE THAN 32 ESTIMATES OPEN, ABEND OCCURS                  *               
*                                                               *               
* PARAM 1     BYTE  0    PRD CODE                               *               
*                  1-3   BUYREC ADDRESS                         *               
*                                                               *               
* PARAM 2           4                                           *               
*                  5-7   TABLE ADDRESS (1024X'00' FIRST TIME)   *               
*                                                               *               
* PARAM 3           8    ON RETURN X'00' = EST FOUND            *               
*                                  X'FF' = NOT FOUND            *               
*                  9-11  SPWORK ADDRESS                         *               
*                                                               *               
*****************************************************************               
         SPACE 2                                                                
SPESTCHK CSECT                                                                  
         NMOD1 0,SPESTCHK                                                       
         L     RA,8(R1)                                                         
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
         USING SPWORKD,RA,R9                                                    
*                                                                               
         LR    R2,R1               SAVE PARAM POINTER                           
         MVI   8(R2),0             CLEAR ERROR IND                              
         L     R3,0(R1)            GET A(REC)                                   
* TEST EST NUM IN TABLE                                                         
         LA    R5,X'80'            SET MASK                                     
         SLL   R5,24               GET IT IN LEFTMOST BIT                       
         L     R1,4(R2)            POINT TO TABLE                               
ESTCHK2  CLI   0(R1),0             TEST E-O-L                                   
         BE    ESTCHK4                                                          
         CLC   0(1,R1),9(R3)       TEST MATCH                                   
         BE    ESTCHK10                                                         
         LA    R1,1(R1)                                                         
         SRL   R5,1                SHIFT MASK                                   
         LTR   R5,R5                                                            
         BNZ   ESTCHK2                                                          
         DC    H'0'                TABLE FULL                                   
*                                                                               
ESTCHK4  MVC   0(1,R1),9(R3)       SET EST NUM IN TABLE                         
*                                                                               
ESTCHK10 DS    0H                                                               
         ZIC   R4,0(R2)            GET PRD CODE                                 
         CLI   0(R2),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R2),220                                                        
         BNH   *+6                                                              
         DC    H'0'                                                             
         BCTR  R4,0                                                             
         SLL   R4,2                X 4                                          
         A     R4,4(R2)            ADD TABLE ADDRESS                            
         LA    R4,32(R4)           AND ADD DSPL TO FIRST ENTRY                  
         ST    R5,ESTMASK          SAVE MASK BIT                                
         NC    ESTMASK,0(R4)       TEST EST FOUND PREVIOUSLY                    
         BNZ   ESTCHKX             YES - EXIT                                   
* MUST READ FOR EST KEY                                                         
         ST    R5,ESTMASK          SAVE MASK BIT AGAIN                          
         MVC   ESTCHKSV,KEY        SAVE CONTENTS OF KEY                         
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),0(R3)      A-M/CLT                                      
         MVC   KEY+7(1),9(R3)      EST                                          
         L     R6,ADCLT                                                         
         USING CLTHDR,R6                                                        
         LA    R6,CLIST                                                         
         DROP  R6                                                               
*                                                                               
ESTCHK12 CLC   3(1,R6),0(R2)                                                    
         BE    ESTCHK14                                                         
         LA    R6,4(R6)                                                         
         CLI   0(R6),C' '                                                       
         BH    ESTCHK12                                                         
         DC    H'0'                                                             
*                                                                               
ESTCHK14 MVC   KEY+4(3),0(R6)      PRD CODE                                     
         MVI   8(R2),X'FF'         PRESET EST NOT FOUND                         
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   *+14                                                             
         OC    0(4,R4),ESTMASK     'OR' IN MASK                                 
         MVI   8(R2),0             AND RESET ERROR FLAG                         
* RESTORE KEY FOR SEQ READING                                                   
         MVC   KEY,ESTCHKSV                                                     
         GOTO1 HIGH                                                             
*                                                                               
ESTCHKX  XMOD1 1                                                                
*                                                                               
ESTCHKSV DS    XL20                                                             
ESTMASK  DS    F                                                                
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
*                                                                               
SPK102   CSECT                                                                  
* READS ESTIMATE RECORDS FOR ALL PRODUCTS IN BRDLIST                            
ESTVAL   NTR1  BASE=*,LABEL=*                                                   
         USING SPWORKD,RA,R9                                                    
*                                                                               
         MVI   EVERROR,C'N'                                                     
*                                                                               
         LA    R4,BRDLIST                                                       
         USING TOPRDD,R4                                                        
         LA    R3,MAXBRDS          UP TO 5 PRODUCTS                             
*                                                                               
EV20     DS    0H                                                               
         MVI   EVPCOUNT,X'01'     FIRST PRODUCT IN POTENTIAL PAIR               
         CLI   0(R4),0            END OF BRDLIST?                               
         BE    EV60                                                             
         MVC   EVPCODE,TOPRD1     CURRENT PRODUCT CODE TO CHECK                 
*                                                                               
EV22     L     R1,ADCLT           LOOK UP 3-CHAR PRODUCT CODE                   
         LA    R1,CLIST-CLTHDR(R1)                                              
*                                                                               
EV30     CLC   EVPCODE,3(R1)                                                    
         BE    EV40                                                             
         LA    R1,4(R1)                                                         
         CLI   0(R1),C' '                                                       
         BH    EV30                                                             
         DC    H'0'               PRODUCT NOT FOUND IN CLIST                    
*                                                                               
EV40     DS    0H                 R1 POINTS TO 3-CHAR PRODUCT CODE HERE         
         MVC   ESTVALSV,KEY        SAVE CONTENTS OF KEY                         
         XC    KEY,KEY                                                          
         L     R2,ADEST                                                         
         MVC   KEY(13),0(R2)       CURRENT ESTIMATE KEY                         
         MVC   KEY+4(3),0(R1)      PRD                                          
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    EV45                                                             
*                                  NO ESTIMATE RECORD - PRINT ERROR             
         MVI   EVERROR,C'Y'                                                     
         LA    R2,P                                                             
         MVC   P(10),=CL10'ESTIMATE'                                            
         EDIT  BEST,(3,P+10)                                                    
         MVC   P+15(20),=CL20'NOT OPEN'                                         
         GOTO1 REPORT                                                           
*                                                                               
EV45     DS    0H                                                               
*        XC    KEY,KEY                                                          
*        MVC   KEY(13),ESTVALSV                                                 
*        GOTO1 HIGH                RESTORE KEY                                  
*        CLC   KEY(13),KEYSAVE                                                  
*        BE    *+6                                                              
*        DC    H'0'                KEY RESTORE FAILED                           
*                                                                               
         CLI   EVPCOUNT,X'01'      ARE WE ON PRODUCT 1 OF 2?                    
         BNE   EV50                NO - ADVANCE TO NEXT LINE IN BRDLIST         
         MVI   EVPCOUNT,X'02'      SEE, IF THERE IS A PRODUCT PAIR              
         CLI   TOPRD2,X'00'                                                     
         BE    EV50                NO PAIR - NEXT LINE IN BRDLIST               
         MVC   EVPCODE,TOPRD2                                                   
         B     EV22                                                             
*                                                                               
EV50     LA    R4,TOPRDL(R4)       NEXT PRODUCT                                 
         BCT   R3,EV20                                                          
*                                                                               
EV60     DS    0H                                                               
         CLI   EVERROR,C'N'                                                     
         JE    EQXIT                                                            
         J     NEQXIT                                                           
*                                                                               
EVPCOUNT DS    X                                                                
EVPCODE  DS    X                                                                
ESTVALSV DS    XL20                                                             
EVERROR  DS    X                                                                
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
*                                                                               
UNLINED  DSECT                                                                  
*                                                                               
UNMKT    DS    CL4                                                              
         DS    CL2                                                              
UNSTA    DS    CL8                                                              
         DS    CL1                                                              
UNEST    DS    CL3                                                              
         DS    CL1                                                              
UNLIN    DS    CL3                                                              
UNCUTIN  DS    CL2                                                              
UNPRD1   DS    0CL16     +24       99999 9999999.99                             
UNPRD1SP DS    CL5            COL  2...3....3....4.                             
         DS    CL1                 5   0    5    0                              
UNPRD1DL DS    CL10                                                             
         DS    CL1                                                              
UNPRD2   DS    CL16      +40                                                    
         DS    CL1                                                              
UNPRD3   DS    CL16      +56                                                    
         DS    CL1                                                              
UNPRD4   DS    CL16      +72                                                    
         DS    CL1                                                              
UNPRD5   DS    CL16      +88                                                    
         DS    CL1                                                              
UNPRD6   DS    CL16      +104                                                   
         SPACE 2                                                                
SPLINED  DSECT                     PRINT LINE FOR PRODUCT CHANGE RPT            
*                                                                               
SPEST    DS    CL3                                                              
         DS    CL1                                                              
SPESTNM  DS    CL20                                                             
         DS    CL1                                                              
SPDAY    DS    CL8                                                              
         DS    CL1                                                              
SPTIM    DS    CL11                                                             
         DS    CL1                                                              
SPPRG    DS    CL18                                                             
         DS    CL1                                                              
SPLEN    DS    CL3                                                              
         DS    CL2                                                              
SPDAT    DS    CL5                                                              
         DS    CL2                                                              
SPPRD    DS    CL3                                                              
         DS    CL1                                                              
SPPRDNM  DS    CL20                                                             
         DS    CL1                                                              
SPSPT    DS    CL4                                                              
         DS    CL2                                                              
SPCOST   DS    CL10                                                             
         EJECT                                                                  
*                                                                               
*                                                                               
         EJECT                                                                  
* AFTER PRINT OFF INCLUDE SPGENAGY/SPGENCLT/SPGENMKT/SPGENBUY                   
*                         SPREPMODES/SPREPWORKD/SPMEDBLOCK/SPGENEST             
*                         SPGENPRD/SPGENSTA                                     
         PRINT OFF                                                              
       ++INCLUDE SPGENAGY                                                       
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENMKT                                                       
       ++INCLUDE SPGENBUY                                                       
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPMEDBLOCK                                                     
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE FALOCKETD                                                      
LKKEYD   DSECT                                                                  
LKBARTYQ EQU   C'BA'               BRAND ALLOCATION LOCK                        
         ORG   LOCKKEY                                                          
LKBAMED  DS    CL1                 MEDIA CODE                                   
LKBACLT  DS    CL3                 CLIENT CODE                                  
LKBAEST  DS    CL3                 ESTIMATE NUMBER (SPACES=ALL)                 
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016SPREPK102 04/04/11'                                      
         END                                                                    
