*          DATA SET MZEIB15    AT LEVEL 075 AS OF 05/01/02                      
*PHASE T21115A,+0                                                               
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
         CLC   =C'NXPLOD',8(R2)                                                 
         BE    BC05                                                             
         MVI   ERRCD,NORECALL                                                   
         OC    SVKEY+14(4),SVKEY+14                                             
         BZ    BUYERR                                                           
         MVC   KEY,SVKEY                                                        
         LA    RE,REC                                                           
         ST    RE,AREC                                                          
         GOTO1 GETREC                                                           
BC05     XC    PRDLIST,PRDLIST     CLEAR ADDED PRD LIST                         
* ON ENTRY R2 HAS A(FIRST ACTV FLDHDR)                                          
         XC    FSTOPS,FSTOPS                                                    
         MVI   FSTOPS,C','                                                      
         LA    R4,8(R2)                                                         
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
         GOTO1 FLDVAL                                                           
*                                                                               
         CLC   =C'NXPLOD',0(R4)                                                 
         BE    B900                                                             
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
         CLC   KEY(12),KEYSAVE                                                  
         BE    *+6                 THIS ASSUMES NO DEL OF EXPL BUYS             
         DC    H'0'                                                             
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
         MVC   ELEM(L'BUYINP1),8(R2)                                            
         MVI   8(R2),C'*'                                                       
         MVC   9(L'BUYINP1-1,R2),ELEM                                           
         FOUT  (R2)                                                             
*                                                                               
         CLI   BUYMSG,0                                                         
         BNE   *+10                                                             
         MVC   BUYMSG(22),=C'** ACTION COMPLETED **'                            
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
         LA    RE,1(R4)            POINT TO DATE                                
         CLI   0(R4),C'+'                                                       
         BE    B204                                                             
         CLI   0(R4),C'-'                                                       
         BNE   NEQXIT                                                           
         CLI   1(R4),C'-'          ALLOW --JAN04 AS INPUT                       
         BNE   B204                                                             
         LA    RE,1(RE)                                                         
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
* UPDATE REC                                                                    
         XC    ELEM,ELEM                                                        
         MVI   ELEM,X'0C'                                                       
         MVI   ELEM+1,10                                                        
         MVC   ELEM+2(2),BUELDT                                                 
         CLI   BUEXPKEY,0          TEST NTWK OR EXPLODED BUY                    
         BNE   *+8                                                              
         MVI   ELEM+6,X'20'        SET $0 COST OVRD FLAG IN NTWK ONLY           
         MVC   ELEM+7(3),BUELCOS                                                
         MVI   ERRCD,BADCOST                                                    
         OC    BUELCOS,BUELCOS                                                  
         BNZ   BUYERR                                                           
         CLI   BUELPRD,0                                                        
         BE    B208X                                                            
         MVI   ELEM+1,14                                                        
         MVC   ELEM+10(1),BUELPRD                                               
         MVC   ELEM+11(1),BUELSEC                                               
         CLI   BUELPR2,0                                                        
         BE    *+20                                                             
         MVI   ELEM+1,18                                                        
         MVC   ELEM+14(1),BUELPR2                                               
         MVC   ELEM+15(1),BUELSEC2                                              
*                                                                               
B208X    MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0D'                                                     
         LA    R6,BDELEM                                                        
B210     LR    R5,R6               SAVE PRV REGEL ADDR                          
         BAS   RE,NEXTEL                                                        
         BNE   B212                                                             
         CLC   ELEM+2(2),2(R6)     TEST OTO DATE TO ELEM                        
         BH    B210                IF OTO IS HIGH, CONTINUE                     
         BL    B212                IF LOW, INSERT IT                            
* EQUAL - CHECK FOR A MINUS OTO WE CAN DELETE                                   
         CLC   ELEM(6),0(R6)       TEST OTO CD/LEN/DATE/PAY DATE                
         BNE   B210                NO                                           
         TM    6(R6),X'80'         IS IT A MINUS OTO                            
         BZ    B210                NO                                           
*------------------------------------------------------*                        
         B     B210                NOP FOR NEW -- LOGIC                         
*------------------------------------------------------*                        
* TEST IDENTICAL ELEMENTS                                                       
         MVC   WORK(32),0(R6)      MOVE EL TO WORK AREA                         
         NI    WORK+6,X'7F'        TURN OFF MINUS SPOT IND                      
         IC    RE,WORK+1                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   WORK(0),ELEM  *EXECUTED*                                         
         BNE   B210                                                             
* MAKE SURE MINUS OTO ISN'T A MAKEGOOD                                          
         TM    6(R5),X'02'         TEST MG IND IN PRV REGEL                     
         BO    B210                YES - DON'T DELETE -OTO                      
* DELETE -OTO AND RESET MINUSSED IND IN REGEL                                   
         NI    6(R5),X'BF'                                                      
         GOTO1 VRECUP,DMCB,BUYREC,(R6)                                          
         B     B214                                                             
B212     DS    0H                                                               
         GOTO1 VRECUP,DMCB,BUYREC,ELEM,(R6)                                     
*                                                                               
B214     CLI   FSTOP,C','          TEST MORE OTO'S                              
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
         TM    6(R6),X'40'         TEST ALREADY MINUSSED                        
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
         ZIC   R0,1(R6)                                                         
         AR    R6,R0               PUT OTO FATER ELEM                           
         GOTO1 VRECUP,DMCB,BUYREC,ELEM,(R6)                                     
         ZIC   R0,1(R6)            POINT TO NEXT ELEM                           
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
B402     MVI   EDTVAL,ELDTEDT                                                   
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
*                                                                               
* ALL DATA NOW IN BUELDATA - FIND ELEM AND UPDATE REC                           
*                                                                               
B410     MVI   ERRCD,NOCOSTOR                                                   
         TM    BUELPRSW,X'20'      TEST COST O/R                                
         BO    BUYERR                                                           
         CLI   BUEXPKEY,0          TEST EXPL BUY                                
         BE    B411                                                             
* FIND NTWK ALLOCATION (IN REC3)                                                
         CLI   BUELNUM,0                                                        
         BNE   *+8                                                              
         MVI   BUELNUM,1                                                        
         BAS   RE,FNDELR3                                                       
         ST    R6,BUEXPEL          SAVE ELEM ADDR                               
*                                                                               
         BAS   RE,SETELDT          ADJUST DATE BY REL DAYS                      
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
         CLI   BUEXPKEY,0          TEST EXPL BUY                                
         BE    B412N               NO                                           
         TM    BUELPRSW,X'04'      NEW ALLO HIATUS                              
         BO    B412N                                                            
* TEST IF BUY HAS A CUT-IN                                                      
         CLI   1(R6),10            IS EXPL SPOT ALLOCATED                       
         BE    B412N               NO                                           
         L     R7,BUEXPEL                                                       
         CLC   1(1,R6),1(R7)       SAME ELEM LEN                                
         BNE   B422                NO - KEEP ALLOC                              
         CLC   10(1,R6),10(R7)     SAME PRD 1                                   
         BNE   B422                NO - KEEP ALLOC                              
         CLI   1(R6),14                                                         
         BNH   B412N                                                            
         CLC   14(1,R6),14(R7)     SAME PRD 2                                   
         BNE   B422                NO - KEEP ALLOC                              
*                                                                               
*                                                                               
B412N    BAS   RE,TESTBP                                                        
         BNE   NEQXIT                                                           
         BAS   RE,TESTMTCH                                                      
         BNE   NEQXIT                                                           
         MVI   ERRCD,MADEGOOD                                                   
         TM    6(R6),X'02'                                                      
         BO    NEQXIT                                                           
         MVI   ERRCD,MISSED                                                     
         TM    6(R6),X'40'                                                      
         BO    NEQXIT                                                           
         TM    BUELPRSW,X'20'      TEST OVRD ENTERED THIS TIME                  
         BZ    B412X               NO                                           
         MVC   7(3,R6),BUELCOS                                                  
         OI    6(R6),X'20'                                                      
         MVI   ERRCD,NOCOSHIA                                                   
         TM    6(R6),X'04'         TEST HIATUS                                  
         BO    NEQXIT                                                           
* TEST COST OVRD = BUY DESC COST                                                
B412X    CLI   BUEXPKEY,0          TEST EXPL BUY                                
         BNE   *+12                YES                                          
         OI    6(R6),X'20'         FORCE COST OVRD IND                          
         B     B413                                                             
*                                                                               
         TM    6(R6),X'20'                                                      
         BZ    B413                                                             
         CLC   7(3,R6),BDCOST                                                   
         BNE   B413                                                             
         NI    6(R6),X'DF'         UNSET COST OVRD IND                          
         XC    7(3,R6),7(R6)        AND CLEAR COST OVRD                         
*                                                                               
B413     CLI   BUELPRD,0           TEST NEW ALLOC ENTERED                       
         BNE   B413N               YES                                          
         TM    BUELPRSW,X'04'      TEST NEW ALLO = HIAT                         
         BO    B413N                                                            
         TM    BUELPRSW,X'20'      TEST COST OVRD THIS TIME                     
         BZ    B413N               NO - MUST BE UNALL                           
         B     B422                FORGET ABOUT CASE OF M=UNALL                 
* CREATE NEW ELEM                                                               
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
* ADD NEW ONE                                                                   
         GOTO1 VRECUP,DMCB,BUYREC,ELEM,(R6)                                     
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
*                                                                               
* MASPRD FORMATS ARE M=DC, M=DC,JAN01   M=DC,JAN01-JAN15                        
*                                                                               
*                                                                               
B610     XC    STDTP,STDTP         SET DEFAULT DATES                            
         MVC   ENDDTP,=X'FFFF'                                                  
         LA    R4,4(R4)                                                         
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
         XC    BUELDATA,BUELDATA                                                
         MVI   EDTVAL,ALLOEDT                                                   
         GOTO1 CALLEDT                                                          
         MVI   ERRCD,NOCOSTOR                                                   
         TM    BUELPRSW,X'20'      TEST COST O/R                                
         BO    BUYERR                                                           
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
* TEST FOR '-W' INPUT                                                           
*                                                                               
B614WK   ST    R4,FADDR                                                         
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
         MVC   WORK+6(6),WORK      RESTORE END DATE                             
* WORK OUT ACTUAL DATES                                                         
B615     MVC   WORK(2),SVSTART     MOVE EST START YEAR                          
         CLC   SVSTART(2),SVEND    EST ALL IN ONE YEAR                          
         BE    B617                YES                                          
         CLC   WORK+2(4),SVSTART+2   INPUT MMDD TO ES ST MMDD                   
         BNL   *+10                  IF INPUT HI OR EQ USE START YR             
         MVC   WORK(2),SVEND                                                    
*                                                                               
B617     PACK  DUB,WORK(2)                                                      
         CLC   WORK+2(4),WORK+8    ST MMDD TO END MMDD                          
         BNH   *+10                                                             
         AP    DUB,=P'1'                                                        
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+6(2),DUB                                                    
*                                                                               
         MVI   ERRCD,STENDERR                                                   
         CLC   WORK(6),WORK+6                                                   
         BH    NEQXIT                                                           
*                                                                               
         CLI   BUWKS,0             TEST WEEKS INPUT                             
         BE    B617X               NO                                           
* CALCULATE END DATE                                                            
         ZIC   R0,BUWKS                                                         
         B     B617BCT                                                          
*                                                                               
B617A    GOTO1 VADDAY,DMCB,WORK+6,WORK+12,7                                     
         MVC   WORK+6(6),WORK+12                                                
B617BCT  BCT   R0,B617A                                                         
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
*                                                                               
* NO DATES ENTERED - CHECK FOR M=HIAT OR M=$ - BOTH ARE INVALID                 
*                                                                               
B620     MVI   ERRCD,NOALLHIA                                                   
         TM    BUELPRSW,X'24'      TEST COST OVRD OR HIAT                       
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
B625     CLI   SVCPROF+0,C'0'      PAID - TEST BRAND POL                        
         BE    B624                NO-SKIP ELEM                                 
         CLC   BDMASPRD(2),BUELPRD IF ANY PAID ELEMS CANT CHANGE MASPRD         
         BE    B624                                                             
         B     BUYERR                                                           
B627     MVI   ERRCD,MISSED                                                     
         TM    6(R6),X'C2'         TEST MINUS,MINUSSED,MADE-GOOD                
         BNZ   B625                YES - SKIP IF POL                            
         BAS   RE,TESTMTCH                                                      
         BNE   B625                                                             
* TEST ELEM IN REQUESTED PERIOD BEFORE CHANGING IT                              
         CLC   2(2,R6),STDTP                                                    
         BL    B624                                                             
         CLC   2(2,R6),ENDDTP                                                   
         BH    B624                                                             
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
         SPACE 2                                                                
* INPUT IS M=HIAT OR M=COST OVRD                                                
* ALLOW IT UNLESS AFFECTED SPOTS ARE BILLED/PAID/MATCHED                        
*                                                                               
B640     BAS   RE,NEXTEL                                                        
         BNE   B425                                                             
         TM    6(R6),X'04'         TEST HIATUS                                  
         BO    B640                YES - SKIP                                   
         CLC   2(2,R6),STDTP                                                    
         BL    B640                                                             
         CLC   2(2,R6),ENDDTP                                                   
         BH    B640                                                             
         BAS   RE,TESTBP                                                        
         BNZ   B625                                                             
         BAS   RE,TESTMTCH                                                      
         BNE   B625                                                             
         MVI   ERRCD,MISSED                                                     
         TM    6(R6),X'C2'         TEST MINUS/MINUSSED/MADE-GOOD                
         BNZ   B625                                                             
*                                                                               
         B     B628                                                             
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
         LA    R1,REC2-REC                                                      
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
         CLC   KEY(12),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
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
B740     LA    R7,L'SVCUTLST(R7)                                                
         LA    R0,SVCUTDTS                                                      
         CR    R7,R0                                                            
         BL    B722                                                             
*                                                                               
         MVI   RCLOPT,RCLCUT                                                    
         B     BCX                                                              
*                                                                               
**********************************************************************          
*                                                                               
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
         IC    R0,0(R2)            AND R2 POINTS AT FIRST ALLOCATION            
         AR    R2,R0                                                            
*                                                                               
B810     ZIC   R5,SVCUTELX         GET LAST ELEMENT NUMBER                      
         ZIC   R0,SVCUTEL                                                       
         SR    R5,R0               SET FOR BCT ON NUMBER OF ELEMENTS            
         LA    R4,SVCUTLST+8       POINT TO FIRST PRODUCT SLOT                  
*                                                                               
B812     TM    1(R2),X'20'         ONLY WANT UNPROTECTED FIELDS                 
         BZ    B812A                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     B812                                                             
         TM    4(R2),X'20'         ONLY NEED TO UPDATE SVCUTLST IF USER         
         BO    B814                HAS MODIFIED THE UNPROTECTED FIELD           
B812A    MVI   EDTVAL,ALLOEDT                                                   
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
         BE    B813                SVCUTLST AND SET CHANGED INDICATOR           
         MVC   1(5,R4),BUELPRD                                                  
         OI    0(R4),X'10'                                                      
*                                                                               
B813     LA    R4,6(R4)            NEXT SLOT                                    
B814     ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCT   R5,B812                                                          
*                                                                               
**********************************************************************          
*                                                                               
B820     L     R0,AREC             SAVE NETWORK BUY IN REC3                     
         LA    R1,REC2-REC                                                      
         L     RE,AREC3                                                         
         LR    RF,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
**********************************************************************          
*                                                                               
B826     OC    5(4,R7),5(R7)                                                    
         BZ    B840                                                             
         XC    KEY,KEY                                                          
         MVC   KEY(13),SVKEY                                                    
         MVC   KEY+4(5),0(R7)      SET MARKET/STATION                           
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
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
*                                                                               
         MVI   RCLOPT,RCLZCUT                                                   
         B     BCX                                                              
         EJECT                                                                  
*                                                                               
***********************************************************************         
*                                                                               
         USING VTIAD,R5                                                         
B900     L     R5,VTIA               CLEAR STATION AND BUYLINE TABLES           
         LA    RE,10                                                            
B905     XC    0(250,R5),0(R5)                                                  
         LA    R5,250(R5)                                                       
         BCT   RE,B905                                                          
*                                                                               
         L     R5,VTIA               INITIALIZE END OF STATION AND              
         MVI   STATAB,X'FF'          BUYLINE TABLES                             
         MVI   BUYTAB,X'FF'                                                     
         MVI   ELEM,0                INITIALIZE BUYLINE COUNTER                 
*                                                                               
         LA    R7,BUYTAB                                                        
         XC    KEY,KEY               READ ALL BUYLINES FOR STATION              
         MVC   KEY(10),SVKEY         (UNTIL CHANGE OF ESTIMATE)                 
         GOTO1 HIGH                  INTO AREC5                                 
         B     B915                                                             
B914     GOTO1 SEQ                                                              
B915     CLC   KEY(10),KEYSAVE                                                  
         BNE   B991                                                             
         MVC   AREC,AREC5                                                       
         GOTO1 GETREC                                                           
         USING BUYREC,R6                                                        
         L     R6,AREC5                                                         
         CLC   BDNRGN,SVNRGN                                                    
         BNE   B914                                                             
         DROP  R6                    SET 'BUYTAB MUST BE UPDATED' FLAG          
         MVI   BYTE,C'N'             TO NO                                      
*                                                                               
         LA    R6,BDELEM-BUYREC(R6)  READ BUYLINE'S 68 ELEMENTS ...             
         MVI   ELCDLO,X'68'          MATCH UP MARKET-STATION TO SVNDEF          
         MVI   ELCDHI,X'68'                                                     
B930     BAS   RE,NEXTEL                                                        
         BNE   B970                                                             
         LA    RE,SVNDEF                                                        
         USING SVNDEFD,RE                                                       
B935     CLC   SVNDMKST,2(R6)                                                   
         BE    B938                                                             
         LA    RE,26(RE)                                                        
         CLC   SVNDMKST,=5X'00'      IF MARKET-STATION NOT FOUND IN             
         BNE   B935                  SVNDEF ... IGNORE THIS 68 ELEMENT          
         B     B930                                                             
*                                                                               
B938     CLC   SVNDPCT,7(R6)        WHEN MARKET-STATIONS MATCHED UP ...         
         BE    B930                 IF PERCENTAGES FROM SVNDEF AND 68           
         MVI   BYTE,C'Y'            ELEMENT MATCH ... GET NEXT 68 ELEM          
         LA    R4,STATAB                                                        
B940     CLI   0(R4),X'FF'                                                      
         BE    B950                                                             
         CLC   0(5,R4),2(R6)                                                    
         BE    B960                 IF PERCENTAGES FROM SVNDEF AND 68           
         LA    R4,L'STATAB(R4)      ELEMENT DON'T MATCH, SET 'BUYTAB            
         B     B940                 MUST BE UPDATED' FLAG TO YES ...            
*                                                                               
B950     MVC   0(5,R4),2(R6)        IF STATION NOT ALREADY IN STATAB            
         MVC   5(4,R4),7(R6)        ... STORE MKT/STA AND OLD COST%             
         MVI   9(R4),X'FF'          (FROM 68 ELEMENT) INTO STATAB ...           
         ZIC   R0,ELEM              MARK END OF STATION TABLE AND BUMP          
         AHI   R0,1                 STATION COUNTER                             
         STC   R0,ELEM                                                          
         B     B930                                                             
*                                                                               
B960     CLC   5(4,R4),7(R6)        IF STATION ALREADY IN STATAB ...            
         BE    B930                 IF PERCENTAGE MATCHES GET NEXT 68           
         B     B914                 ELEM ... IF NOT, SKIP TO NEXT               
*                                   BUYLINE                                     
*                                                                               
***********************************************************************         
*                                                                               
*                                   WHEN NO MORE 68 ELEMENTS IN BUYLINE         
*                                   RECORD ...                                  
*                                                                               
*                                   IF ALL PERCENTAGES FROM 68 ELEMENTS         
B970     CLI   BYTE,C'N'            MATCH PERCENTAGES FROM SVNDEF ...           
         BE    B914                 GET NEXT BUYLINE                            
*                                                                               
         MVC   0(1,R7),KEY+11       IF ANY PERCENTAGES FROM 68 ELEMENTS         
         USING BUYREC,R6            DO NOT MATCH PERCENTAGES FROM               
         L     R6,AREC5             SVNDEF ...                                  
         MVC   1(3,R7),BDCOST       PUT BUYLINE AND OLD SPOT COST INTO          
         LA    R7,L'BUYTAB(R7)      BUYTABLE ...                                
         MVI   0(R7),X'FF'                                                      
*                                                                               
         LA    R6,BDELEM-BUYREC(R6)                                             
         MVI   ELCDLO,X'68'                                                     
         MVI   ELCDHI,X'68'         CHANGE THE PERCENTAGE IN ALL OF THE         
B980     BAS   RE,NEXTEL            BUYLINE'S 68 ELEMENTS TO THE NEW            
         BNE   B990                 VALUE                                       
         LA    RE,SVNDEF                                                        
B981     CLC   SVNDMKST,2(R6)                                                   
         BE    B985                                                             
         LA    RE,26(RE)                                                        
         B     B981                                                             
B985     MVC   7(4,R6),SVNDPCT                                                  
         B     B980                                                             
B990     GOTO1 PUTREC                                                           
         B     B914                                                             
         DROP  RE                                                               
*                                                                               
***********************************************************************         
*                                                                               
*                                                                               
B991     ZIC   R0,ELEM                                                          
         L     RF,VCOMFACS          SORT STATAB BY ASCENDING %                  
         L     RF,CXSORT-COMFACSD(RF)                                           
         GOTO1 (RF),DMCB,(0,STATAB),(R0),9,4,5                                  
*                                                                               
         LA    R4,STATAB                                                        
B994     LA    RE,SVNDEF            FOR EACH STATION, MOVE THE NEW %            
         USING SVNDEFD,RE           INTO ELEM                                   
B994A    CLC   SVNDMKST,0(R4)                                                   
         BE    B994B                                                            
         LA    RE,26(RE)                                                        
         B     B994A                                                            
B994B    MVC   ELEM(4),SVNDPCT                                                  
         DROP  RE                                                               
*                                                                               
         LA    R7,BUYTAB            READ INTO AREC5 ALL THE BUYLINES            
B995     XC    KEY,KEY              FOR EACH STATION                            
         MVC   KEY(10),SVKEY                                                    
         MVC   KEY+4(5),0(R4)                                                   
         MVC   KEY+11(1),0(R7)                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE                                                  
         BNE   B1010                                                            
         GOTO1 GETREC                                                           
*                                                                               
         USING BUYREC,R6                                                        
         L     R6,AREC5                                                         
         SR    R0,R0                                                            
         ZICM  R1,1(R7),3           R1 <--- OLD COST FOR BUYLINE                
         M     R0,ELEM              R1 <--- NEW COST% * OLD COST                
         SR    R0,R0                                                            
         D     R0,=F'100'           R0 <--- NEW COST                            
         LR    R0,R1                                                            
         ZICM  RE,1(R7),3                                                       
         SR    R1,RE                R1 <--- NEW COST - OLD COST                 
         ST    R1,4(R7)             STORE DIFFERENCE INTO BUYTAB                
         STCM  R0,7,1(R7)           STORE NEW COST INTO BUYTAB                  
*                                                                               
         CLI   9(R4),X'FF'          IF AT END OF STATAB, ADD DIFF-              
         BNE   B1000                ERENCE FOR BUYLINE TO THE LAST              
         ZICM  RE,1(R7),3           STATION'S NEW COST                          
         A     RE,4(R7)                                                         
         STCM  RE,7,1(R7)                                                       
*                                                                               
B1000    MVC   BDCOST,1(R7)         MOVE NEW COST INTO RECORD AND PUT           
         GOTO1 PUTREC               IT                                          
*                                                                               
B1010    LA    R7,8(R7)             IF NOT AT END OF BUYLINES, READ             
         CLI   0(R7),X'FF'          NEXT BUYLINE FOR CURRENT STATION            
         BNE   B995                                                             
*                                                                               
         LA    R4,9(R4)             IF AT END OF BUYLINES, BUMP TO NEXT         
         CLI   0(R4),X'FF'          STATION                                     
         BNE   B994                 WHEN NO MORE STATIONS ... DONE              
         B     BCX                                                              
         DROP  R6                                                               
         DROP  R5                                                               
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
         BE    NEXTELX                                                          
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         LTR   R0,R0                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
NEXTEL2  CLI   0(R6),0                                                          
         BE    NEXTELX                                                          
         CLC   ELCDLO,0(R6)                                                     
         BH    NEXTEL                                                           
         CLC   ELCDHI,0(R6)                                                     
         BL    NEXTEL                                                           
         CR    RB,RB                                                            
         B     *+6                                                              
NEXTELX  LTR   RB,RB                                                            
         BR    RE                                                               
         SPACE 2                                                                
* TEST SPOT AT 0(R6), BILLED OR PAID                                            
TESTBP   DS    0H                                                               
         MVI   ERRCD,BLLDPAID                                                   
         OC    4(2,R6),4(R6)                                                    
         BR    RE                                                               
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
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE SPBUYWORK                                                      
 END                                                                            
