*          DATA SET SPBUY15    AT LEVEL 123 AS OF 10/23/13                      
*PHASE T21115A                                                                  
                                                                                
*===============================================================                
* 08SEP06 MHER DO NOT REALLOCATE M= FOR LIMITED TIME PERIOD                     
*               IF ANY NETWORK SPOT IS MATCHED!                                 
* 25APR05 MHER ALLOW NETWORK COST OVERRIDES AND EXPLODE                         
*===============================================================                
                                                                                
         TITLE 'T21115 - SPOTPAK BUY - CANADIAN NTWK ALLOC/OTO'                 
T21115   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21115                                                         
         L     RC,0(R1)                                                         
         USING SPBUYWKD,RC                                                      
*                                                                               
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING T21115+4096,R9                                                   
*                                                                               
         L     RA,VBUYSAVE                                                      
         USING BUYSAVE,RA                                                       
*                                                                               
         L     R3,VBUYTWA                                                       
         USING T211FFD,R3                                                       
*                                                                               
         MVI   AREC4,0             RESET ALLOCATION FLAG                        
         MVI   BUCOST2,X'FF'       SET NO COST VALIDATED YET                    
*                                                                               
         MVI   ERRCD,NORECALL                                                   
         OC    SVKEY+14(4),SVKEY+14                                             
         BZ    BUYERR                                                           
         MVC   KEY,SVKEY                                                        
         LA    RE,REC                                                           
         ST    RE,AREC                                                          
         GOTO1 GETREC                                                           
         XC    PRDLIST,PRDLIST     CLEAR ADDED PRD LIST                         
* ON ENTRY R2 HAS A(FIRST ACTV FLDHDR)                                          
         XC    FSTOPS,FSTOPS                                                    
         MVI   FSTOPS,C','                                                      
         LA    R4,8(R2)                                                         
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
         GOTO1 FLDVAL                                                           
*                                                                               
         CLC   =C'CUT',0(R4)                                                    
         BE    B700                                                             
         CLC   =C'ZCUT',0(R4)                                                   
         BE    B800                                                             
*                                                                               
         MVI   ERRCD,TRCDERR                                                    
         CLI   FLEN+1,1                                                         
         BNE   BUYERR                                                           
         LA    RF,B200                                                          
         CLI   0(R4),C'O'          OTO                                          
         BE    BC10                                                             
         LA    RF,B600                                                          
         CLC   =C'A,M=',0(R4)                                                   
         BE    BC10                                                             
         CLI   0(R4),C'C'                                                       
         BE    BC10                                                             
         LA    RF,B400                                                          
         CLI   0(R4),C'A'                                                       
         BE    BC10                                                             
         B     BUYERR                                                           
*                                                                               
EQXIT    CR    RB,RB                                                            
         B     *+6                                                              
NEQXIT   LTR   RB,RB                                                            
*                                                                               
EXIT     XIT1  1                                                                
*                                                                               
BUYERR   GOTO1 ERROR                                                            
         EJECT                                                                  
BC10     XC    BUEXPDTA,BUEXPDTA   CLEAR WORK AREA                              
         ST    RF,BUEXPSUB         SAVE SUBR ADDR                               
* SAVE REC IN REC3                                                              
         MVC   DUB(4),AREC1        FROM                                         
         MVC   DUB+4(4),AREC3      TO                                           
         GOTO1 MOVEREC                                                          
*                                                                               
         L     RF,BUEXPSUB                                                      
         BASR  RE,RF                                                            
         BNE   BUYERR                                                           
*                                                                               
         L     RE,ASVDARE          DARE TESTS DONE AT ADDEL/DELEL               
         OI    SVDRFLG2-SVDARED(RE),SVDRFLG2_IGN  SET TO IGNORE DARE            
         GOTO1 SETCHGDT                                                         
         MVC   KEY,SVKEY                                                        
         LA    RE,PRDLIST                                                       
         ST    RE,DMCB+20                                                       
         GOTO1 PUTREC                                                           
*                                                                               
         MVC   BUEXPSV,STDTP      SAVE DISPLAY DATES                            
* SEARCH FOR X'68' ELEMS                                                        
         L     R6,AREC3                                                         
         LA    R6,24(R6)                                                        
BC12     MVI   ELCDLO,X'68'                                                     
         MVI   ELCDHI,X'68'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   BCX                                                              
* READ EXPLODED KEY/REC                                                         
         MVC   KEY,SVKEY                                                        
         MVC   KEY+4(5),2(R6)      MKT/STA                                      
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                 THIS ASSUMES NO DEL OF EXPL BUYS             
         DC    H'0'                                                             
*                                                                               
         MVC   BUEXPKEY,KEY        SAVE EXPL KEY                                
         GOTO1 GETREC                                                           
* WORK OUT REL START DATE IF PROGRAM BUY                                        
         XC    BUEXPDAY,BUEXPDAY                                                
         CLI   BDPROGRM,C'='       TEST SIMULCAST                               
         BE    BC18                YES                                          
*                                                                               
         GOTO1 VDATCON,DMCB,(3,BDSTART),WORK+6                                  
         L     R4,AREC3                                                         
         LA    R4,BDSTART-BUYREC(R4)                                            
         GOTO1 (RF),(R1),(3,(R4)),WORK                                          
*                                                                               
         SR    R4,R4               CLEAR COUNTER                                
         LA    R0,1                SET TO ADVANCE DAYS                          
         CLC   WORK(6),WORK+6                                                   
         BE    BC16                                                             
         BL    *+6                                                              
         LCR   R0,R0               UNLESS HIGH ALREADY                          
*                                                                               
BC14     LA    R4,1(R4)            BUMP COUNTER                                 
         GOTO1 VADDAY,DMCB,WORK,WORK+12,(R0)                                    
         MVC   WORK(6),WORK+12                                                  
         CLC   WORK(6),WORK+6                                                   
         BNE   BC14                                                             
BC16     LTR   R0,R0                                                            
         BNM   *+6                                                              
         LCR   R4,R4                                                            
         STH   R4,BUEXPDAY         SET SIGNED RELATIVE DAY                      
* RESET FLDVAL TAB TO RE-EDIT INPUT                                             
BC18     XC    FSTOPS,FSTOPS                                                    
         MVI   FSTOPS,C','                                                      
         LA    R4,8(R2)                                                         
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
         GOTO1 FLDVAL                                                           
*                                                                               
         XC    PRDLIST,PRDLIST                                                  
*                                                                               
         L     RF,BUEXPSUB                                                      
         BASR  RE,RF                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 SETCHGDT                                                         
         LA    RE,PRDLIST                                                       
         ST    RE,DMCB+20                                                       
         MVC   KEY,BUEXPKEY        DMDAPTRS USES DATA IN KEY                    
         GOTO1 PUTREC                                                           
*                                                                               
         B     BC12                                                             
         SPACE 2                                                                
BCX      MVC   KEY,SVKEY           REREAD NTWK BUY                              
         GOTO1 GETREC                                                           
*                                                                               
         MVC   STDTP(4),BUEXPSV    RESTORE DISPLAY DATES                        
         GOTO1 CALLDSP                                                          
*                                                                               
         C     R2,FLAST            TEST DATA IN INPUT AREA                      
         BNH   *+12                YES                                          
         LA    R2,BUYINP1H                                                      
         MVI   BUYINP2H+5,0        SUPPRESS FURTHER INPUT                       
*                                                                               
         LA    R2,BUYINP1H         POINT TO INPUT LINE 1                        
*                                                                               
BCX6     CLI   8(R2),C'*'          TEST PROCESSED PREVIOUSLY                    
         BE    BCX8                                                             
*                                                                               
         MVC   ELEM(L'BUYINP1),8(R2)                                            
         MVI   8(R2),C'*'                                                       
         MVC   9(L'BUYINP1-1,R2),ELEM                                           
*                                                                               
BCX8     FOUT  (R2)                                                             
*                                                                               
         LLC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         C     R2,FLAST            TEST PAST LAST INPUT LINE                    
         BH    EXIT                                                             
         C     R2,FADDR            TEST PAST LAST FIELD PROCESSED               
         BNH   BCX6                                                             
         B     EXIT                                                             
         EJECT                                                                  
* OTO'S                                                                         
*                                                                               
B200     NTR1                                                                   
*                                                                               
         XC    STDTP,STDTP         FORCE DSPLY DATES                            
         MVC   ENDDTP,=X'FFFF'                                                  
*                                                                               
B202     XC    BUELDATA,BUELDATA                                                
         L     R4,FADDR            POINT TO NEXT OTO                            
         AH    R4,FLEN                                                          
         LA    R4,1(R4)                                                         
         MVI   ERRCD,BADOTO                                                     
         LA    RE,2(R4)            POINT TO PRESUMED DATE                       
         CLC   =C'--',0(R4)        ALLOW --JAN04 AS INPUT                       
         BE    B204                                                             
         LA    RE,1(R4)            POINT TO DATE                                
         CLI   0(R4),C'+'                                                       
         BE    B204                                                             
         CLI   0(R4),C'-'                                                       
         BE    B204                                                             
         TM    SVOPT1,SVOPT1_PAIDOTO TEST PAID OTO CODE USED                    
         BZ    NEQXIT                NO                                         
         CLC   SVOTOCHR,0(R4)        TEST FOR SPECIAL PAID -OTO CODE            
         BNE   NEQXIT                                                           
B204     ST    RE,FADDR                                                         
         XC    FLEN,FLEN                                                        
         MVI   EDTVAL,ELDTEDT                                                   
         GOTO1 CALLEDT                                                          
         BAS   RE,SETELDT                                                       
*                                                                               
         OC    STDTP,STDTP         TEST DSPLY DATE SAVED YET                    
         BNZ   *+10                YES                                          
         MVC   STDTP,BUELDT                                                     
         CLC   STDTP,BUELDT        SAVE EARLIEST OTO DATE                       
         BL    *+10                                                             
         MVC   STDTP,BUELDT                                                     
* TEST OTO WITHIN BUY DESC PERIOD                                               
         CLI   BUEXPKEY,0          TEST EXPL                                    
         BNE   B204X               YES-IGNORE                                   
         CLI   SVAPROF+5,C'1'                                                   
         BE    B204X                                                            
         MVI   ERRCD,NOTINPER                                                   
         GOTO1 VDATCON,DMCB,(2,BUELDT),(3,DUB)                                  
         CLC   DUB(3),BDSTART                                                   
         BL    NEQXIT                                                           
         CLC   DUB(3),BDEND                                                     
         BH    NEQXIT                                                           
*                                                                               
B204X    DS    0H                                                               
         CLI   0(R4),C'+'          TEST +OTO                                    
         BNE   B220                NO                                           
* POL +OTO FORMAT IS JAN01XXX OR XXX-YYY/40-20                                  
         L     RE,FADDR                                                         
         AH    RE,FLEN             POINT TO PRD                                 
         ST    RE,FADDR                                                         
         XC    FLEN,FLEN                                                        
* FIRST TEST FOR DATA                                                           
         GOTO1 FLDVAL                                                           
         XC    FLEN,FLEN           SET FOR RE-EDIT                              
         LTR   R5,R5                                                            
         BNE   B205A               YES - GO EDIT                                
         MVI   ERRCD,BADOPOL                                                    
         CLI   SVCPROF+0,C'0'      TEST TRUE POL - MUST HAVE DATA               
         BE    NEQXIT                                                           
         B     B205B                                                            
B205A    MVI   EDTVAL,ALLOEDT                                                   
         GOTO1 CALLEDT                                                          
*                                                                               
B205B    MVI   ERRCD,BADONUM                                                    
         OC    BUELNUM(2),BUELNUM  SHOULD NOT HAVE EL NUM OR NPW                
         BNZ   NEQXIT                                                           
*                                                                               
         CLI   SVCPROF+0,C'0'      TEST BRAND POL                               
         BE    B206                NO                                           
         OC    BUELPRD,BUELPRD     TEST BRAND ENTERED                           
         BNE   B206                YES                                          
         MVC   BUELPRD,BDMASPRD    NO - USE MASPRD FROM BUYLINE                 
         MVC   BUELSEC,BDSEC                                                    
         CLI   BDMASPRD+1,0        TEST P/B MASPRD                              
         BE    B206                NO                                           
         MVC   BUELPR2,BDMASPRD+1                                               
         ZIC   R0,BDSEC                                                         
         SRL   R0,1                                                             
         STC   R0,BUELSEC          ASSUME EVEN SPLIT                            
         STC   R0,BUELSEC2                                                      
* NOW FIND AN ALLOCATED REGEL TO TEST SPLIT                                     
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0B'                                                     
         LA    R6,BDELEM                                                        
         BAS   RE,NEXTEL                                                        
         BNE   B206                                                             
         CLI   1(R6),18                                                         
         BNE   *-12                                                             
         MVC   BUELSEC,11(R6)                                                   
         MVC   BUELSEC2,15(R6)                                                  
*                                                                               
B206     MVI   ERRCD,BADOPOL                                                    
         CLI   SVCPROF+0,C'0'      TEST BRAND POL                               
         BE    B208                NO                                           
         CLI   BUELPRD,0           BRAND POL MUST BE ALLOC                      
         BE    NEQXIT                                                           
         OC    BDMASPRD,BDMASPRD                                                
         BZ    B208                                                             
         MVI   ERRCD,BADOPRD                                                    
         CLC   BDMASPRD(2),BUELPRD NO WRONG BRAND OTO'S                         
         BNE   NEQXIT                                                           
*                                                                               
B208     BAS   RE,SETPRDL          ADD NEW PRD(S) TO LIST                       
*                                                                               
         XC    ELEM,ELEM           UPDATE REC                                   
         MVI   ELEM,X'0C'                                                       
         MVI   ELEM+1,10                                                        
         MVC   ELEM+2(2),BUELDT                                                 
*                                                                               
         TM    BUELPRSW,X'20'      TEST OVRD ENTERED THIS TIME                  
         BZ    B209                NO                                           
*                                                                               
         CLI   BUEXPKEY,0          TEST EXPLODED BUY                            
         BNE   B208B               YES                                          
*                                                                               
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(ONLY1AMT)                                              
         CLI   BUCOST2,X'FF'         TEST FIRST TIME                            
         BE    B208A                                                            
         CLC   BUCOST2+1(3),BUELCOS  TEST SAME COST AMOUNT                      
         BNE   NEQXIT                                                           
*                                                                               
B208A    MVI   BUCOST2,0             RESET FIRST TIME FLAG                      
         MVC   BUCOST2+1(3),BUELCOS  SAVE COST OVERRIDE AMOUNT                  
*                                                                               
         MVC   ELEM+7(3),BUELCOS   COST ALWAYS IN PENNIES!                      
         OI    ELEM+6,X'20'                                                     
         BRAS  RE,ALLODOL          ALLOCATE NTWK DOLS TO STATIONS               
         B     B208X                                                            
*                                                                               
B208B    L     R7,AREC4            FIND STATION IN TABLE                        
         USING SVNPGMD,R7                                                       
*                                                                               
B208C    CLC   SVNPMKST,BUEXPKEY+4  SAME MKT-STA                                
         BE    B208D                                                            
         AHI   R7,SVNPLEN                                                       
         OC    SVNPMKST,SVNPMKST                                                
         BNZ   B208C                                                            
         DC    H'0'                                                             
*                                                                               
B208D    MVC   ELEM+7(3),SVNPSHR+1  MOVE COST                                   
         OI    ELEM+6,X'20'         AND SET COST OVRD IND                       
         DROP  R7                                                               
*                                                                               
B208X    TM    ELEM+6,X'20'                                                     
         BZ    B209                                                             
         CLC   ELEM+7(3),BDCOST    TEST COST OVRD = BUYDESC COST                
         BNE   B209                                                             
         NI    ELEM+6,X'DF'        UNSET COST OVRD IND                          
         XC    ELEM+7(3),ELEM+7    AND CLEAR COST OVRD                          
*                                                                               
B209     CLI   BUELPRD,0                                                        
         BE    B209A                                                            
         MVI   ELEM+1,14                                                        
         MVC   ELEM+10(1),BUELPRD                                               
         MVC   ELEM+11(1),BUELSEC                                               
         CLI   BUELPR2,0                                                        
         BE    B209A                                                            
         MVI   ELEM+1,18                                                        
         MVC   ELEM+14(1),BUELPR2                                               
         MVC   ELEM+15(1),BUELSEC2                                              
*                                                                               
B209A    MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0D'                                                     
         MVC   ELEMDT,ELEM+2       WE ALWAYS KNOW THE DATE                      
         MVI   ELEMNO,1                                                         
         LA    R6,BDELEM                                                        
*                                                                               
B210     LR    R5,R6               SAVE PRV REGEL ADDR                          
         BAS   RE,NEXTEL                                                        
         BNE   B212                                                             
*                                                                               
         CLC   ELEMDT,2(R6)        TEST SAME DATE                               
         BNE   B210A               ONLY NEED TO COUNT ELEMS THIS DATE           
         TM    6(R6),X'80'         IS IT A MINUS OTO                            
         BNZ   B210A                YES - DON'T ADD IT TO COUNT                 
         IC    RE,ELEMNO                                                        
         AHI   RE,1                                                             
         STC   RE,ELEMNO                                                        
*                                                                               
B210A    CLC   ELEM+2(2),2(R6)     TEST OTO DATE TO ELEM                        
         BNL   B210                IF OTO IS HIGH, CONTINUE                     
*                                                                               
B212     GOTO1 VRECUP,DMCB,BUYREC,ELEM,(R6)                                     
         CLI   BUEXPKEY,0          NETWORK CHECK ONLY                           
         BNE   B214                                                             
         L     RE,ASVDARE                                                       
         ST    R6,SVDRELEM-SVDARED(RE)  TEST DARE LOCK                          
         GOTO1 ATESTDAR,DMCB,(RC)                                               
*                                                                               
B214     MVC   SVMAKEDT,ELEMDT     SET DATE/SPOTNUM FOR                         
         MVC   SVMAKENO,ELEMNO     MATCHMAKER                                   
*                                                                               
         CLI   FSTOP,C','          TEST MORE OTO'S                              
         BE    B202                YES                                          
*                                                                               
         BAS   RE,CMTFIX                                                        
         B     B230                GO SET CHG FLAG AND WRITE REC                
         EJECT                                                                  
* POOL -OTO                                                                     
*                                                                               
B220     MVI   ERRCD,BADONUM                                                    
         CLI   BUELPRD,0           MUST NOT ENTER PRD                           
         BNE   NEQXIT                                                           
         CLI   BUELNUM,0           DEFAULT TO SPOT NUM 1                        
         BNE   *+8                                                              
         MVI   BUELNUM,1                                                        
* FIND ELEMENT IN REC                                                           
         BAS   RE,FNDEL                                                         
         CLC   =C'--',0(R4)        TEST REVERSE -OTO                            
         BE    B222                                                             
*                                                                               
         TM    SVOPT1,SVOPT1_PAIDOTO TEST SPECIAL CODES FOR PAID -OTOS          
         BZ    B221                  NO                                         
         CLI   0(R4),C'-'          TEST INPUT IS A -                            
         BNE   B220A               NO                                           
         TM    WRKRUPSW,WRKRUPSW_ISDRMG     IS THIS A DARE MAKEGOOD             
         BO    B221                YES - HOPELESS                               
         BAS   RE,TESTBP           TEST SPOT PAID                               
         BZ    B221                NO                                           
         MVI   ERRCD,PAIDOTO                                                    
         B     BUYERR              YES - NO -OTO                                
*                                                                               
B220A    CLC   SVOTOCHR,0(R4)      TEST INPUT IS SPECIAL -OTO CHAR              
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,TESTBP           TEST SPOT PAID                               
         BNZ   B221                YES - OK                                     
         MVI   ERRCD,UNPDOTO                                                    
         CLI   BUEXPKEY,0          EXPLODED BUY?                                
         BE    BUYERR              NO - REGULAR ERROR                           
         MVI   ERRAREA,X'FE'       YES - $ABEND ERROR                           
         B     BUYERR              NO - SHOULDN'T USE SPECIAL CODE              
*                                                                               
B221     TM    6(R6),X'40'         TEST ALREADY MINUSSED                        
         BO    EQXIT               OK                                           
         CLI   BUEXPKEY,0          TEST EXPLODED BUY                            
         BE    *+12                NO                                           
         TM    6(R6),X'04'         TEST HIATUS                                  
         BO    B230                YES - JUST IGNORE IT                         
         TM    6(R6),X'46'         TEST MINUSSED/MADEGOOD/HIATUS                
         BNZ   NEQXIT                                                           
         CLI   0(R6),X'0C'         IS THE ELEM A +OTO (CANT BE MINUS)           
         BNE   B226                NO                                           
* CHECK FOR A MINUS OTO AGAINST A +OTO THAT CAN BE DELETED                      
* IF SO, JUST DELETE +OTO                                                       
         BAS   RE,TESTBP                                                        
         BNE   B226                                                             
* DELETE +OTO - THEN GO CHECK FOR AFFIDS, ETC.                                  
         GOTO1 VRECUP,DMCB,BUYREC,(R6)                                          
         L     RE,ASVDARE                                                       
         ST    R6,SVDRELEM-SVDARED(RE)  TEST DARE LOCK                          
         GOTO1 ATESTDAR,DMCB,(RC)                                               
         B     B227                                                             
         SPACE 1                                                                
*=========================================================*                     
* NEW LOGIC TO REVERSE -OTO                               *                     
*=========================================================*                     
         SPACE 1                                                                
B222     DS    0H                                                               
         CLI   BUEXPKEY,0          TEST EXPLODED BUY                            
         BE    *+12                NO                                           
         TM    6(R6),X'04'         TEST HIATUS                                  
         BO    B230                YES - SKIP                                   
*                                                                               
         MVI   ERRCD,MADEGOOD                                                   
         TM    6(R6),X'02'         TEST MADEGOOD                                
         BO    BUYERR                                                           
         MVI   ERRCD,NOTMINUS                                                   
         TM    6(R6),X'40'         TEST ALREADY MINUSSED                        
         BZ    BUYERR              CAN'T REVERSE IF NOT MINUS                   
         NI    6(R6),X'FF'-X'40'   UNSET MINUSSED                               
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),X'0C'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,TESTBP           BETTER TO TEST IF -OTO IS PAID               
         BNE   BUYERR              RATHER THAN SPOT !                           
*                                                                               
         L     RE,ASVDARE                                                       
         ST    R6,SVDRELEM-SVDARED(RE)  TEST DARE LOCK                          
         GOTO1 ATESTDAR,DMCB,(RC)                                               
         GOTO1 VRECUP,DMCB,BUYREC,(R6)  AND DELETE THE -OTO                     
         B     B227                                                             
*                                                                               
* BUILD AND INSERT -OTO                                                         
*                                                                               
B226     MVC   ELEM(18),0(R6)      PRESERVE ELEM                                
         XC    ELEM+4(2),ELEM+4    CLEAR PAY DATE                               
         XC    ELEM+12(2),ELEM+12    AND BILL DATES                             
         XC    ELEM+16(2),ELEM+16                                               
         OI    6(R6),X'40'         SET MINUSSED FLAG IN MISSED SPOT             
         OI    ELEM+6,X'80'        SET MINUS SPOT IN OTO                        
         MVI   ELEM,X'0C'          SET OTO EL CODE                              
*                                                                               
         L     RE,ASVDARE                                                       
         LA    RF,ELEM                                                          
         ST    RF,SVDRELEM-SVDARED(RE)  TEST DARE LOCK                          
         GOTO1 ATESTDAR,DMCB,(RC)                                               
*                                                                               
         ZIC   R0,1(R6)                                                         
         AR    R6,R0               PUT OTO AFTER ELEM                           
         GOTO1 VRECUP,DMCB,BUYREC,ELEM,(R6)                                     
         CLI   BUEXPKEY,0          NETWORK CHECK ONLY                           
         BNE   B226A                                                            
         L     RE,ASVDARE                                                       
         ST    R6,SVDRELEM-SVDARED(RE)  TEST DARE LOCK                          
         GOTO1 ATESTDAR,DMCB,(RC)                                               
B226A    ZIC   R0,1(R6)            POINT TO NEXT ELEM                           
         AR    R6,R0                                                            
B227     CLI   0(R6),X'10'         TEST AFFID                                   
         BNE   B228                NO                                           
         GOTO1 VRECUP,DMCB,BUYREC,(R6)                                          
*                                                                               
B228     CLI   FSTOP,C','                                                       
         BE    B202                                                             
*                                                                               
*                                                                               
B230     MVI   BUWHY,X'10'          SET LAST CHG CODE                           
         MVI   RCLOPT,RCLROT                                                    
         B     EQXIT                                                            
         EJECT                                                                  
* ALLOCATIONS                                                                   
*                                                                               
B400     NTR1                                                                   
*                                                                               
         XC    STDTP,STDTP                                                      
         MVC   ENDDTP,=X'FFFF'                                                  
*                                                                               
         CLI   FSTOP,C','          TEST DATA IN INPUT AREA                      
         BNE   B405                NO                                           
* DATA IN INPUT AREA                                                            
B402     TM    SVXFRCTL,SVXFR_SDT+SVXFR_MAK                                     
         BZ    B402W                                                            
         L     RE,FADDR                                                         
         AH    RE,FLEN                                                          
         CLI   0(RE),C','                                                       
         BNE   B402W                                                            
         XC    FLEN,FLEN                                                        
         LA    R4,1(RE)                                                         
         ST    R4,FADDR                                                         
         GOTO1 FLDVAL                                                           
         XC    FLEN,FLEN                                                        
*                                                                               
B402W    MVI   EDTVAL,ELDTEDT                                                   
         GOTO1 CALLEDT                                                          
*                                                                               
         L     R4,FADDR            UPDATE POINTER TO PRD                        
         AH    R4,FLEN                                                          
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
         MVI   EDTVAL,ALLOEDT                                                   
         GOTO1 CALLEDT                                                          
         B     B410                                                             
*                                                                               
* DATA IN DSPLY AREA                                                            
*                                                                               
B405     MVI   ERRCD,TRCDERR                                                    
         LA    R0,BUYINP1H         MUST BE ON INPUT LINE 1                      
         CR    R2,R0                                                            
         BNE   NEQXIT                                                           
         CLI   SVRCLOPT,RCLROT     LAST ACTN SHOULD BE RCL ROT                  
         BE    B407                                                             
         CLI   SVRCLOPT,RCLPAY     OR RCL PAY                                   
         BE    B407                                                             
         B     NEQXIT                                                           
*                                                                               
B407     LA    R2,BUYOUTH                                                       
*                                                                               
* FIND ALLOCATION AND PRECEDING FIELD                                           
*                                                                               
B408     ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    B425                                                             
         CLI   0(R2),9                                                          
         BNH   B408                                                             
         TM    1(R2),X'20'         TEST                                         
         BZ    *+12                                                             
         ST    R2,WORK2            SAVE PROT FLD ADDR                           
         B     B408                                                             
*                                                                               
         ST    R2,WORK2+4          SAVE UNP FLD ADDR                            
         TM    4(R2),X'20'         TEST CHANGED                                 
         BO    B408                NO-IGNORE                                    
         CLI   5(R2),0             TEST NO INPUT                                
         BE    B408                YES - IGNORE                                 
* EDIT DATE FIELD (PRO)                                                         
         L     R2,WORK2                                                         
         LA    R4,8(R2)                                                         
         CLI   0(R4),C'+'                                                       
         BNE   *+8                                                              
         LA    R4,1(R4)                                                         
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
         MVI   EDTVAL,ELDTEDT                                                   
         GOTO1 CALLEDT                                                          
* EDIT PRODUCT FIELD                                                            
         L     R2,WORK2+4                                                       
         LA    R4,8(R2)                                                         
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
         MVI   EDTVAL,ALLOEDT                                                   
         GOTO1 CALLEDT                                                          
         MVI   ERRCD,BADCOMMA                                                   
         CLI   FSTOP,C','                                                       
         BE    NEQXIT                                                           
                                                                                
*=================================================================              
* ALL DATA NOW IN BUELDATA - FIND ELEM AND UPDATE REC                           
*=================================================================              
                                                                                
B410     CLI   BUEXPKEY,0          TEST EXPL BUY                                
         BE    B411                                                             
* FIND NTWK ALLOCATION (IN REC3)                                                
         CLI   BUELNUM,0                                                        
         BNE   *+8                                                              
         MVI   BUELNUM,1                                                        
         BAS   RE,FNDELR3                                                       
         ST    R6,BUEXPEL          SAVE ELEM ADDR                               
*                                                                               
         BAS   RE,SETELDT          ADJUST DATE BY REL DAYS                      
*                                                                               
B411     CLI   SVCPROF+0,C'0'      TEST BRAND POL                               
         BE    B412                NO                                           
* BRAND POL                                                                     
         CLI   BUELPRD,0           TEST NO PRD ENTERED                          
         BNE   *+12                                                             
         TM    BUELPRSW,X'24'      OK IF COST OVRD OR HIAT                      
         BNZ   B412                                                             
         CLC   BDMASPRD(2),BUELPRD  MUST MATCH MASPRD                           
         BE    B412                                                             
         OC    BDMASPRD,BDMASPRD   UNLESS NO MASPRD                             
         BZ    B412                                                             
         MVI   ERRCD,INVBRPOL                                                   
         B     NEQXIT                                                           
*                                                                               
B412     OC    STDTP,STDTP         SAVE EARLIEST ALLOCATION DATE                
         BNZ   *+10                * FOR DISPLAY                                
         MVC   STDTP,BUELDT                                                     
         CLC   STDTP,BUELDT                                                     
         BNH   *+10                                                             
         MVC   STDTP,BUELDT                                                     
*                                                                               
         CLI   BUELNUM,0                                                        
         BNE   *+8                                                              
         MVI   BUELNUM,1                                                        
         BAS   RE,FNDEL                                                         
*                                                                               
         BAS   RE,TESTBP                                                        
         BNE   NEQXIT                                                           
         BAS   RE,TESTMTCH                                                      
         BNE   NEQXIT                                                           
*                                                                               
         MVI   ERRCD,MADEGOOD                                                   
         TM    6(R6),X'02'                                                      
         BO    NEQXIT                                                           
*                                                                               
         MVI   ERRCD,MISSED                                                     
         TM    6(R6),X'40'                                                      
         BO    NEQXIT                                                           
*                                                                               
         TM    BUELPRSW,X'20'      TEST OVRD ENTERED THIS TIME                  
         BZ    B413                NO                                           
*                                                                               
         MVI   ERRCD,NOCOSHIA                                                   
         TM    6(R6),X'04'         TEST HIATUS                                  
         BO    NEQXIT                                                           
*                                                                               
         CLI   BUEXPKEY,0          TEST EXPLODED BUY                            
         BNE   B412C               YES                                          
*                                                                               
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(ONLY1AMT)                                              
         CLI   BUCOST2,X'FF'         TEST FIRST TIME                            
         BE    B412A                                                            
         CLC   BUCOST2+1(3),BUELCOS  TEST SAME COST AMOUNT                      
         BNE   NEQXIT                                                           
B412A    MVI   BUCOST2,0             RESET FIRST TIME FLAG                      
         MVC   BUCOST2+1(3),BUELCOS  SAVE COST OVERRIDE AMOUNT                  
*                                                                               
         MVC   7(3,R6),BUELCOS                                                  
         OI    6(R6),X'20'                                                      
         BRAS  RE,ALLODOL          ALLOCATE NTWK DOLS TO STATIONS               
         B     B412X                                                            
*                                                                               
B412C    L     R7,AREC4            FIND STATION IN TABLE                        
         USING SVNPGMD,R7                                                       
*                                                                               
B412D    CLC   SVNPMKST,BUEXPKEY+4  SAME MKT-STA                                
         BE    B412E                                                            
         AHI   R7,SVNPLEN                                                       
         OC    SVNPMKST,SVNPMKST                                                
         BNZ   B412D                                                            
         DC    H'0'                                                             
*                                                                               
B412E    MVC   7(3,R6),SVNPSHR+1   MOVE COST                                    
         OI    6(R6),X'20'         AND SET COST OVRD IND                        
         DROP  R7                                                               
*                                                                               
B412X    TM    6(R6),X'20'                                                      
         BZ    B413                                                             
         CLC   7(3,R6),BDCOST      TEST COST OVRD = BUYDESC COST                
         BNE   B413                                                             
         NI    6(R6),X'DF'         UNSET COST OVRD IND                          
         XC    7(3,R6),7(R6)       AND CLEAR COST OVRD                          
*                                                                               
B413     CLI   BUELPRD,0           TEST NEW ALLOC ENTERED                       
         BNE   B413N               YES                                          
         TM    BUELPRSW,X'04'      TEST NEW ALLO = HIAT                         
         BO    B413N                                                            
         TM    BUELPRSW,X'20'      TEST COST OVRD THIS TIME                     
         BZ    B413N               NO - MUST BE UNALL                           
         B     B422                FORGET ABOUT CASE OF M=UNALL                 
                                                                                
* CREATE NEW ELEM *                                                             
                                                                                
B413N    BAS   RE,SETPRDL          ADD NEW PRD(S) TO LIST                       
         MVC   ELEM(18),0(R6)                                                   
         NI    ELEM+6,X'20'        TURN OFF ALL BITS EXC. COST OVRD             
         OC    ELEM+6(1),BUELPRSW  'OR' IN NEW INDS                             
         MVI   ELEM+1,10                                                        
         CLI   BUELPRD,0                                                        
         BNE   B414                                                             
         MVI   ERRCD,ALLOCOTO                                                   
         CLI   0(R6),X'0B'         MAY NOT HIAT OR UNALL +OTO OR MG             
         BNE   NEQXIT                                                           
         OC    BDMGDATE,BDMGDATE                                                
         BNZ   NEQXIT                                                           
         TM    ELEM+6,X'04'        TEST NEW ALLO = HIATUS                       
         BZ    B420                NO                                           
         CLI   BUEXPKEY,0          TEST NTWK BUY                                
         BE    B420                YES - LEAVE $0 COST OVRD                     
         NI    ELEM+6,X'DF'        UNSET COST OVRD IND                          
         XC    ELEM+7(3),ELEM+7    AND CLEAR COST OVRD                          
         B     B420                                                             
*                                                                               
B414     MVI   ELEM+1,14                                                        
         MVC   ELEM+10(1),BUELPRD                                               
         MVC   ELEM+11(1),BUELSEC                                               
         XC    ELEM+12(2),ELEM+12                                               
         CLI   BUELPR2,0                                                        
         BE    B420                                                             
         MVI   ELEM+1,18                                                        
         MVC   ELEM+14(1),BUELPR2                                               
         MVC   ELEM+15(1),BUELSEC2                                              
         XC    ELEM+16(2),ELEM+16                                               
                                                                                
* DELETE OLD ELEM                                                               
                                                                                
B420     GOTO1 VRECUP,DMCB,BUYREC,(R6)                                          
*                                                                               
         TM    SVCOPT3,COP3SPOD    TEST SPODS ALLOWED                           
         BZ    B421                                                             
         CLI   BUELPRSW,0          TEST DOING ANYTHING SPECIAL                  
         BNE   *+12                YES - DON'T DELETE IT                        
         CLI   BUELPRD,0           TEST UNALLOCATED                             
         BE    B422                UNALLOCATED SPOD GETS DELETED !              
         CLI   ELEM+1,14           TEST ONE ALLOCATION                          
         BNE   B421                                                             
         CLC   BDSEC,ELEM+11       TEST SLN IS BUYLINE SLN                      
         BE    B421                                                             
         OI    BDSTAT3,BDST3_SPODS SET FLAG FOR SPODS IN BUY                    
                                                                                
* ADD NEW ONE                                                                   
                                                                                
B421     GOTO1 VRECUP,DMCB,BUYREC,ELEM,(R6)                                     
*                                                                               
B422     C     R2,FLAST            ARE WE IN INPUT AREA                         
         BH    B408                NO - LOOK FOR MORE FIELDS                    
         CLI   FSTOP,C','                                                       
         BE    B402                                                             
*                                                                               
B425     MVI   BUWHY,X'20'          SET LAST CHG CODE                           
         MVI   RCLOPT,RCLROT                                                    
         MVC   ENDDTP,=X'FFFF'     SET TO DISPLAY TO E-O-R                      
         B     EQXIT                                                            
         EJECT                                                                  
B600     NTR1                                                                   
*                                                                               
         CLI   2(R4),C'M'          MASPRD                                       
         BE    B610                                                             
         DC    H'0'                                                             
                                                                                
* MASPRD FORMATS ARE M=DC, M=DC,JAN01   M=DC,JAN01-JAN15                        
                                                                                
B610     XC    STDTP,STDTP         SET DEFAULT DATES                            
         MVC   ENDDTP,=X'FFFF'                                                  
         LA    R4,4(R4)                                                         
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
         XC    BUELDATA,BUELDATA                                                
         MVI   EDTVAL,ALLOEDT                                                   
         GOTO1 CALLEDT                                                          
*                                                                               
         MVI   ERRCD,BADOPRD                                                    
         CLI   SVCPROF+0,C'0'      TEST BRAND POL                               
         BE    B612                NO                                           
         CLI   SVPOLPRD,0          TEST BRD POL BY BRD                          
         BNE   B610X               YES                                          
         CLI   FSTOP,C','          TEST DATES PRESENT                           
         BNE   B620                NO                                           
         OC    BDMASPRD,BDMASPRD   IF NO MASPRD, FORGET THIS LOGIC              
         BZ    B614                                                             
                                                                                
* OTHERWISE INPUT MUST BE HIAT OR = BDMASPRD                                    
                                                                                
B610X    CLI   BUELPRD,0                                                        
         BNE   B611                                                             
         TM    BUELPRSW,X'24'      TEST COST OVRD OR HIAT                       
         BZ    NEQXIT                                                           
         B     B612                                                             
B611     CLC   BDMASPRD,BUELPRD                                                 
         BNE   NEQXIT                                                           
*                                                                               
B612     CLI   FSTOP,C','          COMMA MEANS DATES PRESENT                    
         BE    B614                                                             
         MVI   ERRCD,BADCOMMA                                                   
         CLI   FSTOP,0                                                          
         BNE   NEQXIT                                                           
         B     B620                                                             
*                                                                               
B614     MVI   BUWKS,0             RESET IND                                    
         L     R4,FADDR                                                         
         LH    R5,FLEN                                                          
         LA    R4,1(R4,R5)                                                      
         LTR   R5,R5               IF FLEN RESET, NO STOP CHAR                  
         BNZ   *+6                                                              
         BCTR  R4,0                                                             
         GOTO1 VDATVAL,DMCB,(1,(R4)),WORK                                       
         L     R5,0(R1)                                                         
         MVI   ERRCD,PERERR                                                     
         LTR   R5,R5                                                            
         BZ    NEQXIT                                                           
         MVC   WORK+6(6),WORK                                                   
         AR    R4,R5                                                            
         CLI   0(R4),C'-'                                                       
         BE    B614A                                                            
         CLI   0(R4),C' '                                                       
         BH    NEQXIT                                                           
         B     B615                                                             
*                                                                               
B614A    LA    R4,1(R4)                                                         
         GOTO1 VDATVAL,DMCB,(1,(R4)),WORK+6                                     
         L     R5,0(R1)                                                         
         LTR   R5,R5                                                            
         BZ    B614WK                                                           
*                                                                               
         AR    R4,R5                                                            
         OI    0(R4),C' '                                                       
         CLI   0(R4),C' '                                                       
         BNE   NEQXIT                                                           
         B     B615                                                             
*                                                                               
B614WK   ST    R4,FADDR            TEST FOR -W INPUT                            
         XC    FLEN,FLEN                                                        
         MVI   FSTOPS,C'W'                                                      
         GOTO1 FLDVAL                                                           
         CLI   FSTOP,C'W'                                                       
         BNE   NEQXIT                                                           
         TM    FVAL,X'08'          TEST NUMERIC                                 
         BZ    NEQXIT                                                           
         LTR   R5,R5                                                            
         BZ    NEQXIT                                                           
         CVB   R0,DUB                                                           
         LTR   R0,R0                                                            
         BZ    NEQXIT                                                           
         STC   R0,BUWKS            SAVE NUMBER OF WEEKS                         
         LA    R4,1(R4,R5)         POINT PAST STOP CHAR                         
         OI    0(R4),C' '                                                       
         CLI   0(R4),C' '                                                       
         BNE   NEQXIT                                                           
         MVC   WORK+6(6),WORK    RESTORE END DATE DESTROYED BY DATVAL           
                                                                                
* WORK OUT ACTUAL DATES                                                         
                                                                                
B615     MVC   WORK(2),SVSTART     MOVE EST START YEAR                          
         CLC   SVSTART(2),SVEND    EST ALL IN ONE YEAR                          
         BE    B617                YES                                          
         CLC   WORK+2(4),SVSTART+2   INPUT MMDD TO ES ST MMDD                   
         BNL   *+10                  IF INPUT HI OR EQ USE START YR             
         MVC   WORK(2),SVEND                                                    
*                                                                               
B617     MVC   WORK+6(2),WORK      MOVE START YY TO END YY                      
         CLC   WORK+2(4),WORK+8    ST MMDD TO END MMDD                          
         BNH   B617A                                                            
         GOTO1 VADDAY,DMCB,(C'Y',WORK+6),WORK+6,F'1' ADD 1 TO END YY            
*                                                                               
B617A    MVI   ERRCD,STENDERR                                                   
         CLC   WORK(6),WORK+6                                                   
         BH    NEQXIT                                                           
*                                                                               
         CLI   BUWKS,0             TEST WEEKS INPUT                             
         BE    B617X               NO                                           
* CALCULATE END DATE                                                            
         ZIC   R0,BUWKS                                                         
         B     B617BCT                                                          
*                                                                               
B617B    GOTO1 VADDAY,DMCB,WORK+6,WORK+12,7                                     
         MVC   WORK+6(6),WORK+12                                                
B617BCT  BCT   R0,B617B                                                         
*                                                                               
B617X    DS    0H                                                               
         MVI   ERRCD,ESPERERR                                                   
         CLC   WORK(6),SVSTART                                                  
         BL    NEQXIT                                                           
         CLC   WORK+6(6),SVEND                                                  
         BH    NEQXIT                                                           
*                                                                               
         LH    R0,BUEXPDAY                                                      
         LTR   R0,R0                                                            
         BZ    B618                                                             
* ADD RELATIVE DAY                                                              
         GOTO1 VADDAY,DMCB,WORK,WORK+12,(R0)                                    
         MVC   WORK(6),WORK+12                                                  
         GOTO1 (RF),(R1),WORK+6,WORK+12,(R0)                                    
         MVC   WORK+6(6),WORK+12                                                
*                                                                               
B618     GOTO1 VDATCON,DMCB,WORK,(2,STDTP)                                      
         GOTO1 (RF),(R1),WORK+6,(2,ENDDTP)                                      
* START AND END DATES MUST BE IN RECORD                                         
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0D'                                                     
         LA    R6,BDELEM                                                        
         MVI   ERRCD,INVDATE                                                    
*                                                                               
B618A    BAS   RE,NEXTEL                                                        
         BNE   NEQXIT                                                           
         CLC   2(2,R6),STDTP                                                    
         BNE   B618A                                                            
         B     *+12                                                             
*                                                                               
B618B    BAS   RE,NEXTEL                                                        
         BNE   NEQXIT                                                           
         CLC   2(2,R6),ENDDTP                                                   
         BNE   B618B                                                            
         B     B621                                                             
                                                                                
* NO DATES ENTERED - CHECK FOR M=HIAT                                           
                                                                                
B620     MVI   ERRCD,NOALLHIA                                                   
         TM    BUELPRSW,X'04'      TEST HIAT                                    
         BNZ   NEQXIT                                                           
         CLI   SVCPROF+0,C'0'      TEST BRD POL                                 
         BE    B621                NO                                           
         MVI   ERRCD,INVBRPOL                                                   
         CLI   BUELPRD,0                                                        
         BE    NEQXIT                                                           
*                                                                               
B621     DS    0H                                                               
         BAS   RE,SETPRDL          ADD NEW PRD(S) TO LIST                       
         XC    ELEM,ELEM                                                        
         MVI   ELEM,X'0B'                                                       
         MVI   ELEM+1,10                                                        
         CLI   BUELPRD,0                                                        
         BE    B622                                                             
         MVI   ELEM+1,14                                                        
         MVC   ELEM+10(1),BUELPRD                                               
         MVC   ELEM+11(1),BUELSEC                                               
         CLI   BUELPR2,0                                                        
         BE    B622                                                             
         MVI   ELEM+1,18                                                        
         MVC   ELEM+14(1),BUELPR2                                               
         MVC   ELEM+15(1),BUELSEC2                                              
B622     MVC   ELEM+6(1),BUELPRSW                                               
*                                                                               
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0D'                                                     
         LA    R6,BDELEM                                                        
*                                                                               
B623     DS    0H                                                               
         TM    BUELPRSW,X'04'      TEST M=HIAT                                  
         BO    B640                YES                                          
         CLI   BUELPRD,0           TEST NO PRD                                  
         BNE   B624                                                             
         TM    BUELPRSW,X'20'      TEST COST OVRD                               
         BO    B640                                                             
*                                                                               
B624     BAS   RE,NEXTEL                                                        
         BNE   B635                                                             
         TM    6(R6),X'04'         TEST HIATUS                                  
         BO    B624                YES - SKIP                                   
         BAS   RE,TESTBP           TEST BILLED/PAID                             
         BE    B627                NO                                           
*                                                                               
B625     CLI   SVCPROF+0,C'0'      PAID - TEST BRAND POL                        
         BE    B624                NO-SKIP ELEM                                 
         CLC   BDMASPRD(2),BUELPRD IF ANY PAID ELEMS CANT CHANGE MASPRD         
         BE    B624                                                             
         B     BUYERR                                                           
                                                                                
B627     CLC   2(2,R6),STDTP       TEST ELEM IN REQ PERIOD                      
         BL    B624                                                             
         CLC   2(2,R6),ENDDTP                                                   
         BH    B624                                                             
*                                                                               
         BAS   RE,TESTMTCH                                                      
         BNE   NEQXIT              EXIT WITH ERROR IF MATCHED SPOT              
*                                                                               
         MVI   ERRCD,MISSED                                                     
         TM    6(R6),X'C2'         TEST MINUS,MINUSSED,MADE-GOOD                
         BNZ   B625                YES - SKIP                                   
*                                                                               
B628     CLC   0(1,R6),ELCDLO      TEST REGEL                                   
         BE    *+12                YES                                          
         CLI   SVCPROF+0,C'0'      TEST BRD POL                                 
         BE    B623                SKIP OTO'S FOR TRUE POL                      
*                                                                               
         TM    BUELPRSW,X'20'      TEST COST OVRD ENTERED                       
         BZ    *+14                NO                                           
         MVC   7(3,R6),BUELCOS                                                  
         OI    6(R6),X'20'          AND COST OVRD IND                           
* TEST COST OVRD = BDCOST                                                       
         CLI   BUEXPKEY,0          TEST NTWK BUY                                
         BE    B629                YES - LEAVE $0 COST OVRD                     
         TM    BUELPRSW,X'04'      TEST M=HIAT                                  
         BO    B628X               YES - CLEAR COST O/R                         
         TM    6(R6),X'20'                                                      
         BZ    B629                                                             
         CLC   7(3,R6),BDCOST                                                   
         BNE   B629                                                             
B628X    NI    6(R6),X'DF'         TURN OFF OVRD IND                            
         XC    7(3,R6),7(R6)       AND RESET COST                               
*                                                                               
B629     MVC   ELEM(1),0(R6)       SET ELEM CODE                                
         MVC   ELEM+2(8),2(R6)     MOVE DATE/INDS/COST                          
         CLI   BUELPRD,0           TEST NO ALLOC ENTERED                        
         BNE   *+12                                                             
         TM    BUELPRSW,X'20'      TEST COST OVRD THIS TIME                     
         BO    B623                YES - IGNORE UNALL REQUEST                   
         NI    ELEM+6,X'20'        DROP ALL BUT COST OVRD                       
         OC    ELEM+6(1),BUELPRSW                                               
         CLC   ELEM(2),0(R6)       TEST SAME LEN                                
         BNE   B631                                                             
B630     ZIC   RE,1(R6)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     B623                                                             
         MVC   0(0,R6),ELEM  *EXECUTED*                                         
*                                                                               
B631     GOTO1 VRECUP,DMCB,BUYREC,(R6)                                          
*                                                                               
         GOTO1 (RF),(R1),BUYREC,ELEM,(R6)                                       
         B     B623                                                             
         EJECT                                                                  
B635     CLI   SVCPROF+0,C'0'      TEST BRD POL                                 
         BNE   *+14                YES                                          
         CLC   STDTP(4),=X'0000FFFF'  TRUE POL - TEST DATES ENTERED             
         BNE   B637                                                             
         CLI   BUELPRD,0           TEST NEW PRD                                 
         BE    B637                NO (HIAT)                                    
         MVC   BDMASPRD(2),BUELPRD                                              
B637     B     B425                                                             
                                                                                
*===========================================================                    
* INPUT IS M=HIAT OR M=COST OVRD                                                
* ALLOW IT UNLESS AFFECTED SPOTS ARE BILLED/PAID/MATCHED                        
*===========================================================                    
                                                                                
B640     MVI   ELCDLO,X'0B'        RESET SEARCH ARGS                            
         MVI   ELCDHI,X'0C'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   B425                                                             
         TM    6(R6),X'04'         TEST HIATUS                                  
         BO    B640                YES - SKIP                                   
         CLC   2(2,R6),STDTP                                                    
         BL    B640                                                             
         CLC   2(2,R6),ENDDTP                                                   
         BH    B640                                                             
         BAS   RE,TESTBP                                                        
         BNZ   NEQXIT                                                           
         BAS   RE,TESTMTCH                                                      
         BNE   NEQXIT                                                           
         MVI   ERRCD,MISSED                                                     
         TM    6(R6),X'C2'         TEST MINUS/MINUSSED/MADE-GOOD                
         BNZ   NEQXIT                                                           
*                                                                               
         CLI   BUEXPKEY,0          TEST EXPLODED BUY                            
         BNE   B642B               YES                                          
*                                                                               
         MVC   7(3,R6),BUELCOS                                                  
         OI    6(R6),X'20'                                                      
         CLI   AREC4,0             TEST ALREADY ALLOCATED                       
         BNE   B642X                                                            
         BRAS  RE,ALLODOL          ALLOCATE NTWK DOLS TO STATIONS               
         B     B642X                                                            
*                                                                               
B642B    L     R7,AREC4            FIND STATION IN TABLE                        
         USING SVNPGMD,R7                                                       
*                                                                               
B642C    CLC   SVNPMKST,BUEXPKEY+4  SAME MKT-STA                                
         BE    B642D                                                            
         AHI   R7,SVNPLEN                                                       
         OC    SVNPMKST,SVNPMKST                                                
         BNZ   B642C                                                            
         DC    H'0'                                                             
*                                                                               
B642D    MVC   7(3,R6),SVNPSHR+1   MOVE COST                                    
         OI    6(R6),X'20'         AND SET COST OVRD IND                        
*                                                                               
B642X    TM    6(R6),X'20'                                                      
         BZ    B644                                                             
         CLC   7(3,R6),BDCOST      TEST COST OVRD = BUYDESC COST                
         BNE   B644                                                             
         NI    6(R6),X'DF'         UNSET COST OVRD IND                          
         XC    7(3,R6),7(R6)       AND CLEAR COST OVRD                          
*                                                                               
B644     B     B640                                                             
         EJECT                                                                  
*========================================================*                      
* EDIT CUT-INS FROM CUT DISPLAY                          *                      
* CUT IN STATIONS ARE IN SVCUTLST                        *                      
*           DATES ARE IN SVCUTDTS                        *                      
* LOW ELEMENT IS IN SVCUTEL, HIGH ELEMENT IS IN SVCUTELX *                      
*                                                        *                      
* ENTRY FOR EACH STATION HAS LENGTH 8+8*6=56             *                      
* MKT/STA (5)  SPARE (3)                                 *                      
*                                                        *                      
* PRD DETAILS REPEAT 8 TIMES                             *                      
* INDS    (1)  X'80' = SPOT MINUS/MINUSSED               *                      
*              X'40' = PAID (CAN'T REALLOCATE)           *                      
*              X'10' = PRODUCTS CHANGED                  *                      
*              X'01' = NO SPOT THIS DATE (FOR THIS STA)  *                      
*                                                        *                      
* PRD1/PRD2/SLN1/SLN2/STATUS                             *                      
*========================================================*                      
         SPACE 1                                                                
B700     DS    0H                                                               
         MVI   ERRCD,NORCLCUT                                                   
         CLI   SVRCLOPT,RCLCUT                                                  
         BNE   BUYERR                                                           
*                                                                               
         LA    R7,SVCUTLST                                                      
         LA    R2,BUYOUTH                                                       
         SR    R0,R0                                                            
B702     IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLC   0(2,R2),=X'1020'    LOOK FOR 16 BYTE PROT DATE FIELD             
         BNE   B702                                                             
*                                                                               
B704     IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLC   0(2,R2),=X'1020'    NOW SKIP ALL THE DATES                       
         BE    B704                                                             
         IC    R0,0(R2)            THEN SKIP FIRST STATION FIELD                
         AR    R2,R0                                                            
*                                                                               
B710     DS    0H                                                               
         ZIC   R5,SVCUTELX         GET LAST ELEMENT NUMBER                      
         ZIC   R0,SVCUTEL                                                       
         SR    R5,R0               SET FOR BCT ON NUMBER OF ELEMENTS            
         LA    R4,8(R7)            POINT TO FIRST PRODUCT SLOT                  
*                                                                               
B712     TM    1(R2),X'20'         TEST PROT                                    
         BO    B714                                                             
         TM    4(R2),X'20'         TEST MODIFIED                                
         BO    B714                NO                                           
         MVI   EDTVAL,ALLOEDT                                                   
         LA    RE,8(R2)                                                         
         ST    RE,FADDR                                                         
         XC    FLEN,FLEN                                                        
         XC    BUEXPDTA,BUEXPDTA   CLEAR OUTPUT AREA                            
         GOTO1 CALLEDT                                                          
*                                                                               
         MVI   ERRCD,NOCOSTOR                                                   
         TM    BUELPRSW,X'20'      TEST COST OV ENTERED                         
         BO    BUYERR                                                           
         CLC   1(5,R4),BUELPRD     TEST SAME PRODUCTS                           
         BE    B714                YES                                          
         MVC   1(5,R4),BUELPRD     MOVE PRDS TO TABLE                           
         OI    0(R4),X'10'         SET CHANGED IND                              
*                                                                               
B714     DS    0H                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         LA    R4,6(R4)            NEXT SLOT                                    
         BCT   R5,B712                                                          
*                                                                               
         LA    R7,L'SVCUTLST(R7)   POINT TO NEXT ENTRY                          
         LA    R0,SVCUTDTS                                                      
         CR    R7,R0               TEST PAST END                                
         BNL   B720                                                             
         OC    0(5,R7),0(R7)       TEST MORE ENTRIES                            
         BZ    B720                NO                                           
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               POINT TO INPUT FOR NEXT STATION              
         B     B710                                                             
         EJECT                                                                  
B720     DS    0H                                                               
         L     R0,AREC             SAVE NETWORK BUY IN REC3                     
         LHI   R1,REC2-REC                                                      
         L     RE,AREC3                                                         
         LR    RF,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         LA    R7,SVCUTLST                                                      
*                                                                               
B722     LA    R4,8(R7)                                                         
         LA    R5,8                                                             
*                                                                               
B724     TM    0(R4),X'10'         TEST CHANGED                                 
         BO    B726                                                             
         LA    R4,6(R4)                                                         
         BCT   R5,B724                                                          
         B     B740                NO CHANGES THIS STATION                      
*                                                                               
B726     XC    KEY,KEY                                                          
         MVC   KEY(13),SVKEY                                                    
         MVC   KEY+4(5),0(R7)      SET MARKET/STATION                           
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 GETREC                                                           
         XC    PRDLIST,PRDLIST     CLEAR ADDED PRD LIST                         
*                                                                               
         LA    R6,BDELEM                                                        
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0D'                                                     
         SR    R5,R5               CLEAR COUNTER                                
*                                                                               
B730     BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                MUST FIND STARTING ELEMENT                   
*                                                                               
         TM    6(R6),X'80'         TEST MINUS SPOT                              
         BO    B730                                                             
         LA    R5,1(R5)            BUMP COUNTER                                 
         CLM   R5,1,SVCUTEL        TEST REACHED FIRST ELEMENT YET               
         BL    B730                                                             
*                                                                               
         LA    R4,8(R7)            POINT TO PRODUCT TABLE                       
         LA    R5,8                SET FOR MAX PRDS                             
*                                                                               
B732     TM    0(R4),X'10'         TEST PRODUCTS CHANGED                        
         BZ    B736                                                             
*                                                                               
         MVC   BUELPRD(2),1(R4)    MOVE NEW ALLOCATIONS                         
         BAS   RE,SETPRDL          UPDATE ADDED PRODUCT LIST                    
*                                                                               
         MVC   ELEM(10),0(R6)      MOVE ELEMENT                                 
         XC    ELEM+10(8),ELEM+10                                               
*                                                                               
         NI    ELEM+6,X'20'        TURN OFF ALL BITS BUT COST OVRD              
         OC    ELEM+6(1),5(R4)     'OR' IN NEW INDS                             
         MVI   ELEM+1,10                                                        
         CLI   BUELPRD,0                                                        
         BE    B734                                                             
*                                                                               
         MVI   ELEM+1,14                                                        
         MVC   ELEM+10(1),1(R4)    MOVE PRD1                                    
         MVC   ELEM+11(1),3(R4)    AND SLN1                                     
         CLI   2(R4),0                                                          
         BE    B734                                                             
         MVI   ELEM+1,18                                                        
         MVC   ELEM+14(1),2(R4)                                                 
         MVC   ELEM+15(1),4(R4)                                                 
*                                                                               
B734     DS    0H                                                               
         GOTO1 VRECUP,DMCB,BUYREC,(R6)        DELETE OLD ELEMENT                
         GOTO1 VRECUP,DMCB,BUYREC,ELEM,(R6)   ADD NEW ELEMENT                   
*                                                                               
B736     DS    0H                                                               
         BAS   RE,NEXTEL           GET NEXT SPOT                                
         BNE   B738                                                             
         TM    6(R6),X'80'         TEST MINUS                                   
         BO    B736                                                             
         LA    R4,6(R4)                                                         
         BCT   R5,B732                                                          
*                                                                               
B738     MVI   BUWHY,X'10'         SET LAST CHANGE CODE                         
         GOTO1 SETCHGDT                                                         
         LA    RE,PRDLIST                                                       
         ST    RE,DMCB+20                                                       
         GOTO1 PUTREC                                                           
*                                                                               
         CLI   PRDLIST,0           TEST ANY PRDS ADDED                          
         BE    B740                                                             
         BRAS  RE,SET68EL                                                       
*                                                                               
B740     LA    R7,L'SVCUTLST(R7)                                                
         LA    R0,SVCUTDTS                                                      
         CR    R7,R0                                                            
         BL    B722                                                             
         MVI   RCLOPT,RCLCUT                                                    
         B     B850                                                             
         EJECT                                                                  
B800     MVI   ERRCD,NORCLCUT                                                   
         CLI   SVRCLOPT,RCLZCUT                                                 
         BNE   BUYERR                                                           
*                                                                               
         LA    R7,SVZCUTST                                                      
         LA    R2,BUYOUTH                                                       
B802     ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLC   0(2,R2),=X'4E20'    SKIP THE 2 78 BYTE PROTECTED STATION         
         BNE   B802                FIELDS ...                                   
B804     IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLC   0(2,R2),=X'4E20'                                                 
         BE    B804                THEN SKIP FIRST DATE FIELD ...               
*                                                                               
B810     ZIC   R5,SVCUTELX         GET LAST ELEMENT NUMBER                      
         ZIC   R0,SVCUTEL                                                       
         SR    R5,R0               SET FOR BCT ON NUMBER OF ELEMENTS            
         LA    R4,SVCUTLST+8       POINT TO FIRST PRODUCT SLOT                  
                                                                                
* R2 POINTS TO A DATE FIELD                                                     
                                                                                
B812     ZIC   R0,0(R2)            ADVANCE TO PRODUCT FIELD                     
         AR    R2,R0                                                            
         TM    1(R2),X'20'         TEST PROTECTED                               
         BO    B816                YES - IGNORE                                 
         TM    4(R2),X'20'         ONLY NEED TO UPDATE SVCUTLST IF USER         
         BO    B816                HAS MODIFIED THE UNPROTECTED FIELD           
*                                                                               
B814     MVI   EDTVAL,ALLOEDT                                                   
         LA    RE,8(R2)                                                         
         ST    RE,FADDR                                                         
         XC    FLEN,FLEN                                                        
         XC    BUEXPDTA,BUEXPDTA   VALIDATE PRODUCT CODE                        
         GOTO1 CALLEDT                                                          
*                                                                               
         MVI   ERRCD,NOCOSTOR                                                   
         TM    BUELPRSW,X'20'      TEST COST OV ENTERED                         
         BO    BUYERR                                                           
         CLC   1(5,R4),BUELPRD     MOVE NEW (VALIDATED) PRODUCT INTO            
         BE    B816                SVCUTLST AND SET CHANGED INDICATOR           
         MVC   1(5,R4),BUELPRD                                                  
         OI    0(R4),X'10'                                                      
*                                                                               
B816     LA    R4,6(R4)            NEXT SLOT                                    
*                                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCT   R5,B812                                                          
*                                                                               
**********************************************************************          
*                                                                               
B820     L     R0,AREC             SAVE NETWORK BUY IN REC3                     
         LHI   R1,REC2-REC                                                      
         L     RE,AREC3                                                         
         LR    RF,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
**********************************************************************          
*                                                                               
B826     OC    4(4,R7),4(R7)                                                    
         BZ    B840                                                             
         XC    KEY,KEY                                                          
         MVC   KEY(13),SVKEY                                                    
         MVC   KEY+4(5),0(R7)      SET MARKET/STATION                           
B827     GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
B827A    GOTO1 GETREC                                                           
         XC    PRDLIST,PRDLIST     CLEAR ADDED PRD LIST                         
*                                                                               
         LA    R6,BDELEM                                                        
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0D'                                                     
         SR    R5,R5               CLEAR COUNTER                                
B830     BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                MUST FIND STARTING ELEMENT                   
         TM    6(R6),X'80'         TEST MINUS SPOT                              
         BO    B830                                                             
         LA    R5,1(R5)            BUMP COUNTER                                 
         CLM   R5,1,SVCUTEL        TEST REACHED FIRST ELEMENT YET               
         BL    B830                                                             
*                                                                               
         LA    R4,SVCUTLST+8       POINT TO PRODUCT TABLE                       
         LA    R5,18               SET FOR MAX PRDS                             
*                                                                               
B832     MVC   BUELPRD(2),1(R4)    MOVE NEW ALLOCATIONS                         
         BAS   RE,SETPRDL          UPDATE ADDED PRODUCT LIST                    
*                                                                               
         MVC   ELEM(10),0(R6)      MOVE ELEMENT                                 
         XC    ELEM+10(8),ELEM+10                                               
*                                                                               
         NI    ELEM+6,X'20'        TURN OFF ALL BITS BUT COST OVRD              
         OC    ELEM+6(1),5(R4)     'OR' IN NEW INDS                             
         MVI   ELEM+1,10                                                        
         CLI   BUELPRD,0                                                        
         BE    B834                                                             
*                                                                               
         MVI   ELEM+1,14                                                        
         MVC   ELEM+10(1),1(R4)    MOVE PRD1                                    
         MVC   ELEM+11(1),3(R4)    AND SLN1                                     
         CLI   2(R4),0                                                          
         BE    B834                                                             
         MVI   ELEM+1,18                                                        
         MVC   ELEM+14(1),2(R4)    MOVE PRD2                                    
         MVC   ELEM+15(1),4(R4)    AND SLN2                                     
*                                                                               
B834     GOTO1 VRECUP,DMCB,BUYREC,(R6)        DELETE OLD ELEMENT                
         GOTO1 VRECUP,DMCB,BUYREC,ELEM,(R6)   ADD NEW ELEMENT                   
*                                                                               
         CLI   PRDLIST,0           TEST ADDED ANY PRDS                          
         BE    B836                                                             
         BRAS  RE,SET68EL          UPDATE NETWORK 68 ELEM                       
*                                                                               
B836     BAS   RE,NEXTEL           GET NEXT SPOT                                
         BNE   B838                                                             
         TM    6(R6),X'80'         TEST MINUS                                   
         BO    B836                                                             
         LA    R4,6(R4)                                                         
         BCT   R5,B832                                                          
*                                                                               
B838     MVI   BUWHY,X'10'         SET LAST CHANGE CODE                         
         GOTO1 SETCHGDT                                                         
         LA    RE,PRDLIST                                                       
         ST    RE,DMCB+20                                                       
         GOTO1 PUTREC                                                           
*                                                                               
B840     LA    R7,L'SVZCUTST(R7)                                                
         LA    R0,SVZCUTSX                                                      
         CR    R7,R0                                                            
         BL    B826                                                             
         MVI   RCLOPT,RCLZCUT                                                   
*                                                                               
B850     TM    AREC3,X'80'         TEST NETWORK BUY UPDATED                     
         BZ    BCX                                                              
         NI    AREC3,X'7F'         UNSET FLAG BEFORE IO USED                    
         MVC   KEY,SVKEY                                                        
         MVC   AREC,AREC1          READ INTO REC 1                              
         GOTO1 GETREC                                                           
         MVC   AREC,AREC3          WRITE FROM REC 3                             
         GOTO1 PUTREC                                                           
         MVC   AREC,AREC1          RESTORE NORMAL IO ADDRESS                    
         B     BCX                                                              
         EJECT                                                                  
*                                                                               
***********************************************************************         
*                                                                               
SETPRDL  NTR1                                                                   
         LA    R4,BUELPRD                                                       
         BAS   RE,TSTPRD                                                        
         LA    R4,BUELPR2                                                       
         BAS   RE,TSTPRD                                                        
         B     EXIT                                                             
*                                                                               
TSTPRD   NTR1                                                                   
         CLI   0(R4),0                                                          
         BE    EXIT                                                             
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0D'                                                     
         LA    R6,BDELEM                                                        
TSTPRD2  BAS   RE,NEXTEL                                                        
         BNE   TSTPRD4                                                          
         ZIC   R0,1(R6)                                                         
         SH    R0,=H'10'                                                        
         BNP   TSTPRD2                                                          
         SRL   R0,2                                                             
         LA    R7,10(R6)                                                        
         CLC   0(1,R7),0(R4)       SAME PRD                                     
         BE    EXIT                                                             
         LA    R7,4(R7)                                                         
         BCT   R0,*-14                                                          
         B     TSTPRD2                                                          
* PRD NOT IN ANY ELEMENT - SEE IF IN LIST YET                                   
TSTPRD4  LA    R7,PRDLIST                                                       
*                                                                               
TSTPRD6  CLI   0(R7),0                                                          
         BE    TSTPRD8                                                          
         CLC   0(1,R7),0(R4)                                                    
         BE    EXIT                                                             
         LA    R7,1(R7)                                                         
         B     TSTPRD6                                                          
* ADD PRD TO LIST                                                               
TSTPRD8  MVC   0(1,R7),0(R4)                                                    
         B     EXIT                                                             
         EJECT                                                                  
FNDELR3  L     R6,AREC3                                                         
         LA    R6,24(R6)                                                        
         B     *+8                                                              
FNDEL    LA    R6,BDELEM                                                        
*                                                                               
         LR    R8,RE               SAVE CALLING REG                             
         MVI   ERRCD,BADSPOT                                                    
         XC    ELEMDT,ELEMDT                                                    
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0D'                                                     
FNDEL2   BAS   RE,NEXTEL                                                        
         BNE   NEQXIT                                                           
         CLC   ELEMDT,2(R6)                                                     
         BE    *+8                                                              
         MVI   ELEMNO,0                                                         
         MVC   ELEMDT,2(R6)                                                     
         IC    RE,ELEMNO                                                        
         TM    6(R6),X'80'                                                      
         BO    *+8                                                              
         LA    RE,1(RE)                                                         
         STC   RE,ELEMNO                                                        
         CLC   ELEMDT,BUELDT                                                    
         BNE   FNDEL2                                                           
         CLC   ELEMNO,BUELNUM                                                   
         BNE   FNDEL2                                                           
         LR    RE,R8                                                            
         BR    RE                                                               
         EJECT                                                                  
SETELDT  NTR1                                                                   
*                                                                               
         LH    R0,BUEXPDAY                                                      
         LTR   R0,R0                                                            
         BZ    EXIT                                                             
         GOTO1 VDATCON,DMCB,(2,BUELDT),WORK                                     
         GOTO1 VADDAY,DMCB,WORK,WORK+6,(R0)                                     
         GOTO1 VDATCON,DMCB,WORK+6,(2,BUELDT)                                   
         B     EXIT                                                             
         EJECT                                                                  
NEXTEL   CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         JNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
NEXTEL2  CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         CLC   ELCDLO,0(R6)                                                     
         JH    NEXTEL                                                           
         CLC   ELCDHI,0(R6)                                                     
         JL    NEXTEL                                                           
         CR    RB,RB                                                            
         J     *+6                                                              
NEXTELX  LTR   RB,RB                                                            
         BR    RE                                                               
         SPACE 2                                                                
* TEST SPOT AT 0(R6), BILLED OR PAID                                            
TESTBP   NTR1                                                                   
         MVI   ERRCD,BLLDPAID                                                   
         OC    4(2,R6),4(R6)                                                    
         BNZ   NEQXIT                                                           
         CLI   BUEXPKEY,0          NETWORK ONLY                                 
         BNE   EQXIT                                                            
         L     RE,ASVDARE                                                       
         ST    R6,SVDRELEM-SVDARED(RE)  TEST DARE LOCK                          
         GOTO1 ATESTDAR,DMCB,(RC)                                               
         B     EQXIT                                                            
         EJECT                                                                  
* TEST FOR AFFIDS FOR ELEM AT 0(R6).                                            
* ASSUME ELCDLO AND ELCDHI CONTAIN REGEL ARGUMENTS                              
*                                                                               
TESTMTCH NTR1                                                                   
         MVI   ERRCD,MATCHED                                                    
*                                                                               
         LR    R5,R6               SAVE EL ADDRESS                              
TMTCH2   BAS   RE,NEXTEL                                                        
         BNE   TMTCH4                                                           
         CLC   2(2,R5),2(R6)       TEST SAME DATE                               
         BE    TMTCH2                                                           
TMTCH4   LR    R7,R6               R7 IS END OF SRCH                            
         BCTR  R7,0                                                             
TMTCH6   CLI   0(R5),X'10'         TEST AFFID                                   
         BE    NEQXIT              EXIT WITH CC NOT EQ IF MATCHED               
         ZIC   R6,1(R5)                                                         
         BXLE  R5,R6,TMTCH6                                                     
         B     EQXIT                                                            
         SPACE 2                                                                
* MAKE SURE COMMENTS LAST ELEMS IN RECORD                                       
*                                                                               
CMTFIX   NTR1                                                                   
*                                                                               
         MVC   DUB(4),AREC1        FROM                                         
         MVC   DUB+4(4),AREC2      TO                                           
         GOTO1 MOVEREC             SAVE REC IN REC2                             
*                                                                               
         MVI   ELCDLO,X'62'                                                     
         MVI   ELCDHI,X'68'                                                     
         LA    R6,BDELEM                                                        
         BAS   RE,NEXTEL                                                        
         BNE   EXIT                NO COMMENTS                                  
*                                                                               
CMTF2    GOTO1 VRECUP,DMCB,BUYREC,(R6)                                          
         BAS   RE,NEXTEL2                                                       
         BE    CMTF2                                                            
* R6 POINTS TO E-O-R                                                            
         LR    R7,R6               SAVE E-O-R ADDRESS                           
         L     R6,AREC2                                                         
         LA    R6,24(R6)                                                        
*                                                                               
CMTF4    BAS   RE,NEXTEL                                                        
         BNE   EXIT                                                             
         GOTO1 VRECUP,DMCB,BUYREC,(R6),(R7)                                     
         ZIC   R0,1(R7)            ADD NEXT ELEM AFTER THIS ONE                 
         AR    R7,R0                                                            
         B     CMTF4                                                            
*                                                                               
         LTORG                                                                  
                                                                                
*=============================================================                  
* UPDATE 68 ELEM WITH CUTIN FLAG                                                
* R7 POINTS TO CUTIN MARKET/STATION                                             
*=============================================================                  
                                                                                
SET68EL  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R6,AREC3                                                         
         LA    R6,0(R6)            CLEAR HOB                                    
         MVI   ELCDLO,X'68'                                                     
         MVI   ELCDHI,X'68'                                                     
         AHI   R6,BDELEM-BUYREC                                                 
*                                                                               
SET68A   BRAS  RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   2(5,R6),0(R7)       MATCH MKT/STA                                
         BNE   SET68A                                                           
         CLI   1(R6),12            TEST NEW ELEM LEN                            
         BL    SET68B              NO - BUILD NEW ELEMENT                       
         TM    11(R6),X'80'        TEST FLAG ON                                 
         BO    SET68X                                                           
         OI    11(R6),X'80'                                                     
         OI    AREC3,X'80'         TEST REC3 WRITE REQD                         
         B     SET68X                                                           
*                                                                               
SET68B   MVC   ELEM(11),0(R6)                                                   
         MVI   ELEM+1,12           SET NEW ELEM LEN                             
         MVI   ELEM+11,X'80'                                                    
         GOTO1 VRECUP,DMCB,(C'S',AREC3),(R6)                                    
         GOTO1 (RF),(R1),,ELEM,(R6)                                             
         OI    AREC3,X'80'         SET REC3 UPDATED                             
*                                                                               
SET68X   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*=================================================================              
* BUILD A TABLE IN AREC4 FROM X'68' ELEMENTS AND ALLOCATE DOLLARS               
*=================================================================              
                                                                                
ALLODOL  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R0,AREC4                                                         
         LHI   R1,REC4X-REC4                                                    
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         MVI   AREC4,C'A'          SET ALLOCATED FLAG                           
*                                                                               
         L     R6,AREC3            NETWORK BUY SAVED IN AREC3                   
         AHI   R6,24                                                            
         MVI   ELCDLO,X'68'                                                     
         MVI   ELCDHI,X'68'                                                     
         USING NTWKELEM,R6                                                      
*                                                                               
         L     R7,AREC4                                                         
         USING SVNPGMD,R7                                                       
*                                                                               
ALLOD2   BRAS  RE,NEXTEL                                                        
         BNE   ALLOD10                                                          
*                                                                               
         MVC   SVNPMKST,NTWKMKST   MKT-STA                                      
         OC    BUELCOS,BUELCOS     TEST COST=0                                  
         BZ    *+10                                                             
         MVC   SVNPSHR,NTWKSHR     COST SHARE                                   
*                                                                               
ALLOD4   AHI   R7,SVNPLEN                                                       
         B     ALLOD2                                                           
         DROP  R6                                                               
*                                                                               
ALLOD10  L     R7,AREC4                                                         
         USING SVNPGMD,R7                                                       
*                                                                               
ALLOD12  ICM   R0,15,SVNPSHR       GET SHARE                                    
         BZ    ALLOD14                                                          
         SR    R1,R1                                                            
         ICM   R1,7,BUELCOS        GET COST (ALWAYS PENNIES)                    
         BZ    ALLODX                                                           
         AR    R1,R1               X 2                                          
         MR    R0,R0               X SHARE                                      
         D     R0,=F'100000'                                                    
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         STCM  R1,15,SVNPSHR                                                    
*                                                                               
ALLOD14  AHI   R7,SVNPLEN                                                       
         OC    SVNPMKST,SVNPMKST                                                
         BNZ   ALLOD12                                                          
                                                                                
         L     R7,AREC4            TEST SUM=BUCOST                              
         SR    R8,R8                                                            
         LR    RE,R7                                                            
*                                                                               
ALLOD20  ICM   R0,15,SVNPSHR                                                    
         AR    R8,R0               SUM COSTS                                    
         CLC   SVNPSHR-SVNPGMD(4,RE),SVNPSHR                                    
         BH    *+6                                                              
         LR    RE,R7               SAVE A(LARGEST)                              
         AHI   R7,SVNPLEN                                                       
         OC    SVNPMKST,SVNPMKST                                                
         BNZ   ALLOD20                                                          
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,7,BUELCOS                                                     
         SR    R8,R1                                                            
         ICM   R0,15,SVNPSHR-SVNPGMD(RE)                                        
         SR    R0,R8                                                            
         STCM  R0,15,SVNPSHR-SVNPGMD(RE)                                        
*                                                                               
ALLODX   J     EXIT                                                             
         LTORG                                                                  
         DROP  R7                                                               
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE SPBUYWORK                                                      
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
 END                                                                            
